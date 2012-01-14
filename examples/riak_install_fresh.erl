-module(riak_install_fresh).

-compile(export_all).

run(Hosts) ->
    %% Hosts is a list; each host has an open SSH connection + associated platform
    [Host | _] = Hosts,
    run_single(Host).

run_single(Host) ->
    case talos_host:cmd(Host, "which riak") of
        {ok, Stdout} ->
            lager:info("~p\n", [Stdout]);
        {error, {Res, Stderr}} ->
            lager:error("Result: ~p Output: ~p\n", [Res, Stderr])
    end,

    %% TODO: What about how the package name is different on solaris?
    lager:info("Riak package installed? ~p\n", [talos_host:package_installed(Host, "riak")]),

    lager:info("~p\n", [talos_host:install_package_file(Host, "/Users/dizzyd/Downloads/be/riak-1.0.2/riak-1.0.2-1.el5.x86_64.rpm")]),
    
    lager:info("~p\n", [talos_host:install_package(Host, "git")]).

    


%%     %% Check for existing install riak or riak command on path
%%     fail_if_any([Platform:cmd(Host, "which riak"),
%%                  Platform:package_installed(Host, "riak")]),

%%     %% Install riak package
%%     Platform:install_package_file(Host, RiakFile),

%%     %% Setup config on riak;
%%     %% ? How do we find out where app.config is ?
%%     RiakConfig = proplists:get_value(riak_config, Config, []),
%%     [host_apply_config(Host, App, Key, Value) || {App, Key, Value} <- RiakConfig],

%%     %% Startup riak via host service (normally init.d)
%%     host_service_up(Host, "riak"),

%%     %% Wait for KV service to come up
%%     wait_for_kv_service(Host),

%%     %% TODO: Wait for ring to stablize ??

%%     %% Run riak-admin test
%%     check(host_cmd(Host, "riak-admin test")).


%% host_cmd(Host, Cmd) ->
%%     (Host#host.platform_module):cmd(Host, Cmd).




