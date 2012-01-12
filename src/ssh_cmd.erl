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
-module(ssh_cmd).

%% Public API
-export([run/2, run/3]).

%% Behaviour for ssh_channel
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2,
         code_change/3]).

run(Conn, Cmd) ->
    run(Conn, Cmd, []).

run(Conn, Cmd, Opts) ->
    ChannelTimeout = proplists:get_value(channel_timeout, Opts, timer:seconds(1)),
    {ok, Channel} = ssh_connection:session_channel(Conn, ChannelTimeout),
    {ok, Pid} = ssh_channel:start_link(Conn, Channel, ?MODULE, [Conn, Channel]),
    ssh_channel:call(Pid, {exec, Cmd, Opts}).

-record(state, { conn,
                 channel,
                 from,
                 eof,
                 exit_status,
                 stdout_buffer = <<>>,
                 stderr_buffer = <<>>}).

init([Conn, Channel]) ->
    {ok, #state{ conn = Conn, channel = Channel }}.

handle_call({exec, Command, Opts}, From, State) ->
    CmdTimeout = proplists:get_value(command_timeout, Opts, timer:seconds(5)),
    set_envs(State, proplists:get_value(env, Opts, [])),
    ssh_connection:exec(State#state.conn, State#state.channel, Command ++ "\n",
                        CmdTimeout),
    {noreply, State#state { from = From }}.

handle_cast(undefined, State) ->
    {noreply, State}.

handle_msg({ssh_channel_up, _Channel, _Conn}, State) ->
    {ok, State}.

handle_ssh_msg({ssh_cm, _Pid, {data, _Channel, 0, Data}}, State) ->
    {ok, State#state { stdout_buffer = <<(State#state.stdout_buffer)/binary, Data/binary>> }};
handle_ssh_msg({ssh_cm, _Pid, {data, _Channel, 1, Data}}, State) ->
    {ok, State#state { stdout_buffer = <<(State#state.stderr_buffer)/binary, Data/binary>> }};
handle_ssh_msg({ssh_cm, _Pid, {exit_status, _Channel, ExitStatus}}, State) ->
    {ok, maybe_reply(State#state { exit_status = ExitStatus })};
handle_ssh_msg({ssh_cm, _Pid, {eof, _Channel}}, State) ->
    {ok, maybe_reply(State#state { eof = true })}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_reply(#state { eof = undefined } = State) ->
    State;
maybe_reply(#state { exit_status = undefined } = State) ->
    State;
maybe_reply(State) ->
    Result = case State#state.exit_status of
                 0 -> ok;
                 _ -> error
             end,
    ssh_channel:reply(State#state.from, {Result, {State#state.exit_status,
                                                  State#state.stdout_buffer,
                                                  State#state.stderr_buffer}}),
    State.

set_envs(State, []) ->
    State;
set_envs(State, [{Key, Value} | Rest]) ->
    ok = ssh_connection:setenv(State#state.conn, State#state.channel, Key, Value,
                               timer:seconds(1)),
    set_envs(State, Rest).
