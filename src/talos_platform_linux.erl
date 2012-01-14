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
-module(talos_platform_linux).

-compile(export_all).

-record(state, { packager,
                 conn,
                 sftp }).

init(Conn) ->
    %% Spin up a SFTP channel
    {ok, Sftp} = ssh_sftp:start_channel(Conn),

    %% Determine if the machines has either a /etc/redhat-release file (rpm-based)
    %% or /etc/debian_version (deb-based)
    IsRedhat = ssh_sftp_x:file_exists(Sftp, "/etc/redhat-release"),
    IsDebian = ssh_sftp_x:file_exists(Sftp, "/etc/debian_version"),
    Packager = if IsRedhat -> rpm;
                  IsDebian -> deb;
                  true     -> unknown
               end,
    lager:info("Packager: ~p\n", [Packager]),
    #state { packager = Packager, conn = Conn, sftp = Sftp }.

package_installed(#state { packager = rpm } = State, PackageName) ->
    case ssh_cmd:run(State#state.conn, "rpm -q " ++ PackageName) of
        {ok, _} ->
            {true, State};
        {error, _} ->
            {false, State}
    end.

install_package_file(#state { packager = rpm } = State, Filename) ->
    TargetFile = filename:join(["/tmp", filename:basename(Filename)]),
    case ssh_sftp_x:cp_to(State#state.sftp, Filename, TargetFile) of
        ok ->
            %% File is copied to remote system -- install that
            case ssh_cmd:run(State#state.conn, "sudo rpm -Uvh " ++ TargetFile) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    {error, {install_error, Reason}, State}
            end;
        {error, Reason} ->
            {error, {scp_error, Reason}, State}
    end.

install_package(#state { packager = rpm } = State, PackageName) ->
    case ssh_cmd:run(State#state.conn, "sudo yum install -y " ++ PackageName) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

remove_package(State, PackageName) ->
    case ssh_cmd:run(State#state.conn, "sudo rpm -e " ++ PackageName) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

service_up(State, Service) ->
    ok.

service_down(State, Service) ->
    ok.

reboot(State) ->
    ok.
