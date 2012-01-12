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
-module(talos_host).

-behaviour(gen_server).

-include("talos.hrl").

%% API
-export([start_link/2,
         connect/1,
         cmd/2,
         package_installed/2,
         install_package_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { host_id,
                 platform,
                 platform_state,
                 platform_id,
                 opts,
                 conn
               }).

%% ====================================================================
%% API
%% ====================================================================

start_link(HostId, Opts) ->
    gen_server:start_link(?MODULE, [HostId, Opts], []).

connect(Pid) ->
    gen_server:call(Pid, connect, infinity).

cmd(Pid, Cmd) ->
    gen_server:call(Pid, {cmd, Cmd}).

package_installed(Pid, PackageName) ->
    gen_server:call(Pid, {package_installed, PackageName}).

install_package_file(Pid, Filename) ->
    gen_server:call(Pid, {install_package_file, Filename}).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([HostId, Opts]) ->
    {ok, #state { host_id = HostId, opts = Opts }}.

handle_call(connect, _From, State) ->
    S2 = maybe_connect(State),

    %% Determine platform identifier for this host
    PlatformId = platform_id(S2),

    %% Use the platform identifier to select the approprate module
    case select_platform(talos_config:get(platforms), PlatformId) of
        undefined ->
            %% No module matched our platform ID; not much we can do
            ?ABORT("No platform module available for host ~s: ~s\n",
                   [S2#state.host_id, PlatformId]);
        Platform ->
            lager:info("Host ~s is using platform module: ~p\n", [S2#state.host_id, Platform]),
            PState = Platform:init(S2#state.conn),
            {reply, ok, S2#state { platform = Platform,
                                   platform_state = PState,
                                   platform_id = PlatformId }}
    end;
handle_call({cmd, Cmd}, _From, State) ->
    Reply = case ssh_cmd:run(State#state.conn, Cmd) of
                {ok, {0, Stdout, _Stderr}} ->
                    {ok, Stdout};
                {error, {Res, _Stdout, Stderr}} ->
                    {error, {Res, Stderr}}
            end,
    {reply, Reply, State};
handle_call({package_installed, PackageName}, _From, State) ->
    {IsInstalled, PState} = (State#state.platform):package_installed(State#state.platform_state,
                                                                     PackageName),
    {reply, IsInstalled, State#state { platform_state = PState }};
handle_call({install_package_file, Filename}, _From, State) ->
    PState = State#state.platform_state,
    case (State#state.platform):install_package_file(PState, Filename) of
        {ok, PState1} ->
            {reply, ok, State#state { platform_state = PState1 }};
        {error, Reason, PState1} ->
            {reply, {error, Reason}, State#state { platform_state = PState1}}
    end.




handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
%% ====================================================================

maybe_connect(#state { conn = undefined } = State) ->
    Ip = case proplists:get_value(ip, State#state.opts) of
             undefined ->
                 case inet:getaddr(State#state.host_id, inet) of
                     {ok, Ip0} ->
                         lager:debug("Resolved host id ~p to IP: ~p\n",
                                     [State#state.host_id, Ip0]),
                         inet_parse:ntoa(Ip0);
                     {error, Reason} ->
                         ?ABORT("Failed to resolve host id ~p: ~p\n",
                                [State#state.host_id, Reason]),
                         undefined
                 end;
             Ip1 ->
                 Ip1
         end,
    Port = proplists:get_value(port, State#state.opts, 22),
    Conn = case ssh:connect(Ip, Port, State#state.opts) of
               {ok, Conn0} ->
                   lager:info("Connected to ~s (~p:~p)\n", [State#state.host_id, Ip, Port]),
                   Conn0;
               {error, Reason1} ->
                   lager:error("Failed to connect to ~p (~p:~p): ~p\n",
                               [State#state.host_id, Ip, Port, Reason1]),
                   undefined
           end,
    State#state { conn = Conn };
maybe_connect(State) ->
    lager:debug("Already connected to ~p via SSH.\n", [State#state.host_id]),
    State.


platform_id(State) ->
    case ssh_cmd:run(State#state.conn, "uname -psmr") of
        {ok, {_Res, Stdout, _Stderr}} ->
            string:strip(binary_to_list(Stdout));
        {error, {Res, _Stdout, Stderr}} ->
            lager:error("Platform ID ~p failed with result code ~p: ~s\n",
                        [State#state.host_id, Res, Stderr]),
            undefined
    end.

%% uname -psmr
%% SunOS 5.11 i86pc i386 - joyent
%% Linux 2.6.18-274.el5 x86_64 x86_64 - centos 5.7
%% Linux 2.6.38-8-server x86_64 x86_64 - ubuntu 11.10
%% Darwin 11.2.0 x86_64 i386 - osx lion

select_platform([], _PlatformId) ->
    undefined;
select_platform([{Regex, Module} | Rest], PlatformId) ->
    case re:run(PlatformId, Regex, []) of
        nomatch ->
            select_platform(Rest, PlatformId);
        {match, _} ->
            Module
    end.
