%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% talos: Distributed Testing Framework
%%
%% Copyright (c) 2012 Basho Technologies, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(talos_core).

-export([run/1]).

-include("talos.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

run(Args) ->
    %% Load lager and initialize to log to console only by default while
    %% the system is bootstrapping
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend, info}]),
    application:set_env(lager, crash_log, undefined),

    %% Initialize lager -- if this fails, we're in bad shape
    case application:start(lager) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("Failed to initialize logging system: ~p\n", [Reason]),
            halt(1)
    end,

    %% Make sure crypto and ssh are up
    application:start(crypto),
    application:start(ssh),

    %% Pre-load the app so that we get default configuration
    ok = application:load(talos),

    %% Parse out command line arguments
    case getopt:parse(options(), Args) of
        {ok, {Options, _}} ->

            %% Merge command-line options into application env
            merge_options(Options),

            %% Look for talos.config in the cwd
            {ok, Cwd} = file:get_cwd(),
            ConfigFile = filename:join([Cwd, "talos.config"]),
            talos_config:load([ConfigFile]),

            %% Save basedir so that tests can access it
            talos_config:set(base_dir, filename:absname(Cwd)),

            %% Init code path
            add_code_paths(talos_config:get(code_paths)),

            %% If a source directory is specified, compile and load all .erl files found
            %% there.
            case talos_config:get(source_dir) of
                [] ->
                    ok;
                SourceDir ->
                    load_source_files(SourceDir)
            end,

            %% Setup working directory for this run. All logs, stats, and config
            %% info will be placed here
            RunId = id(),
            RunDir = filename:join([Cwd, talos_config:get(results_dir), RunId]),
            ok = filelib:ensure_dir(filename:join(RunDir, "foobar")),
            talos_config:set(run_id, RunId),
            talos_config:set(run_dir, RunDir),
            lager:info("Run ID: ~s\n", [RunId]),

            %% Create a link to the run dir for convenience
            RunLink = filename:join([filename:dirname(RunDir), "last"]),
            file:delete(RunLink),
            ok = file:make_symlink(RunDir, RunLink),

            %% Copy the config into the run dir for posterity
            {ok, _} = file:copy(ConfigFile, filename:join(RunDir, filename:basename(ConfigFile))),

            %% Ensure the ssh/ dir exists in the run dir
            ok = filelib:ensure_dir(filename:join([RunDir, "ssh", "foobar"])),

            %% Set our CWD to the run dir
            ok = file:set_cwd(RunDir),

            %% Spin up hosts
            Hosts = start_hosts(talos_config:get(hosts, [])),
            case Hosts of
                [] ->
                    lager:warning("No hosts available for testing!\n");
                _ ->
                    ok
            end,

            %% For each test module, spin up a process on which to run the test
            %% (for controlling timeouts) and wait for the process to exit.
            Tests = talos_config:get(tests, []),
            case Tests of
                [] ->
                    ?ABORT("No tests specified in talos.config!\n", []);
                _ ->
                    ok
            end,
            run_tests(Tests, Hosts);

        {error, {Reason2, Data}} ->
            ?ABORT("getopt error: ~s ~p~n~n", [Reason2, Data])
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%% options accepted via getopt
%%
options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     boolean, "Show the program options"},
     {verbose,  $v, "verbose",  boolean, "Use debug level output"}
    ].

merge_options([]) ->
    ok;
merge_options([{verbose, true} | Rest]) ->
    lager:set_loglevel(lager_console_backend, debug),
    merge_options(Rest);
merge_options([_Option | Rest]) ->
    merge_options(Rest).

add_code_paths([]) ->
    ok;
add_code_paths([Path | Rest]) ->
    [add_code_path(P) || P <- expand_wildcard_code_path(Path)],
    add_code_paths(Rest).

add_code_path(Path) ->
    case code:add_path(Path) of
        true ->
            lager:info("Added code path: ~s\n", [Path]);
        {error, bad_directory} ->
            ?ABORT("Could not add code path entry: ~s\n", [Path])
    end.

expand_wildcard_code_path(Path) ->
    Absname = filename:absname(Path),
    case string:chr(Absname, "*") of
        0 ->
            [Absname];
        _ ->
            filelib:wildcard(Absname)
    end.

load_source_files(Dir) ->
    CompileFn = fun(F, _Acc) ->
                        case compile:file(F, [report, binary, {parse_transform, lager_transform}]) of
                            {ok, Mod, Bin} ->
                                {module, Mod} = code:load_binary(Mod, F, Bin),
                                lager:info("Loaded ~p from ~s\n", [Mod, F]),
                                ok;
                            Error ->
                                ?ABORT("Failed to compile ~s: ~p\n", [F, Error])
                        end
                end,
    filelib:fold_files(Dir, ".*.erl", false, CompileFn, ok).

%%
%% Construct a string suitable for use as a unique ID for this test run
%%
id() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).

%%
%% Startup the host processes; note that we break out the start from the SSH connect
%% so that we can parallelize the connection process.
%%
start_hosts(Hosts) ->
    start_hosts(Hosts, []).

start_hosts([], Acc) ->
    connect_hosts(Acc);
start_hosts([{HostId, Username, Password} | Rest], Acc) ->
    SshOpts = [{user, Username},
               {password, Password},
               {port, 22},
               {user_dir, filename:join([talos_config:get(run_dir), "ssh"])},
               {silently_accept_hosts, true},
               {connect_timeout, talos_config:get(ssh_connect_timeout)}],
    {ok, Pid} = talos_host:start_link(HostId, SshOpts),
    start_hosts(Rest, [Pid | Acc]).


connect_hosts(Hosts) ->
    %% Spawn a process to invoke connect on each host
    PidsMrefs = [spawn_monitor(talos_host, connect, [H]) || H <- Hosts],
    wait_for_connects(PidsMrefs),
    Hosts.


wait_for_connects([]) ->
    ok;
wait_for_connects(PidsMrefs) ->
    receive
        {'DOWN', Mref, process, Pid, Reason} ->
            NewPidsMrefs = lists:delete({Pid, Mref}, PidsMrefs),
            case Reason of
                normal ->
                    ok;
                {error, Reason} ->
                    %% Useful?!
                    lager:error("Connect to host failed: ~p\n", [Reason])
            end,
            wait_for_connects(NewPidsMrefs)
    end.

run_tests([], _Hosts) ->
    ok;
run_tests([Test | Tests], Hosts) ->
    Start = now(),
    {Pid, Mref} = spawn_monitor(fun() -> Test:run(Hosts) end),
    receive
        {'DOWN', Mref, process, Pid, Reason} ->
            ElapsedSecs = timer:now_diff(now(), Start) / 1000000,
            case Reason of
                normal ->
                    lager:info("Test ~p completed in ~p seconds\n",
                               [Test, ElapsedSecs]),
                    run_tests(Tests, Hosts);
                Error ->
                    ?ABORT("Test ~p failed in ~p seconds: ~p\n",
                           [Test, ElapsedSecs, Error])
            end
    end.
