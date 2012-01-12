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
-module(ssh_sftp_x).

-define(BLOCK_SIZE, 1048576). %% 1 MB

%% Extensions for ssh_sftp module
-export([file_exists/2,
         cp_to/3]).

file_exists(ChannelPid, Filename) ->
    case ssh_sftp:read_file_info(ChannelPid, Filename) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

cp_to(ChannelPid, LocalFilename, RemoteFilename) ->
    case file:open(LocalFilename, [read, binary, read_ahead, raw]) of
        {ok, LocalFile} ->
            case ssh_sftp:open(ChannelPid, RemoteFilename, [write, creat, append, binary]) of
                {ok, RemoteFile} ->
                    try
                        Read = fun() -> file:read(LocalFile, ?BLOCK_SIZE) end,
                        Write = fun(Data) -> ssh_sftp:write_file(ChannelPid, RemoteFile, Data) end,
                        copy_blocks(LocalFilename, Read, RemoteFilename, Write)
                    after
                        file:close(LocalFile),
                        ssh_sftp:close(ChannelPid, RemoteFile)
                    end;
                {error, Reason} ->
                    {error, {RemoteFilename, Reason}}
            end;
        {error, Reason} ->
            {error, {LocalFilename, Reason}}
    end.


copy_blocks(ReadFile, ReadFn, WriteFile, WriteFn) ->
    case ReadFn() of
        {ok, Data} ->
            case WriteFn(Data) of
                ok ->
                    copy_blocks(ReadFile, ReadFn, WriteFile, WriteFn);
                {error, Reason} ->
                    {error, {WriteFile, Reason}}
            end;
        eof ->
            ok;
        {error, Reason} ->
            {error, {ReadFile, Reason}}
    end.
