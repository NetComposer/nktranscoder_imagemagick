%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 NetScale, SL.  All Rights Reserved.
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
-module(nktranscoder_imagemagick_util).
-export([thumbnail/4]).

thumbnail(SrvId, CT, File, Params) ->
    SpanId = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?MODULE, SrvId, "NkTranscoder::ImageMagick::Thumbnail", SpanId),
    Format = maps:get(format, Params, []),
    Syntax = #{
        image => #{
            size => integer
        }
    },
    case nklib_syntax:parse(Format, Syntax) of
        {ok, Parsed, []} ->
            Image = maps:get(image, Parsed, #{}),
            Size = maps:get(size, Image, 1024),
            #{max_conversion_time:=Time} = nkserver:get_config(SrvId),
            nkserver_ot:log(?MODULE, "options parsed: ~p", [Parsed]),
            lager:notice("Starting thumbnail conversion (CT:~p, time:~p size:~p", [CT, Time, Size]),
            nkserver_ot:log(?MODULE, <<"writing file">>),
            BaseName = <<"/tmp/trans-", (nklib_util:luid())/binary>>,
            try
                ok = file:write_file(<<BaseName/binary, ".jpeg">>, File),
                nkserver_ot:log(?MODULE, <<"starting conversion">>),
                Cmd = list_to_binary([
                    "convert ",
                    BaseName, ".jpeg ",
                    "-auto-orient ",
                    "-thumbnail 250x", integer_to_binary(Size), " ",
                    "-unsharp 0x.5 ",
                    BaseName, ".png"
                ]),
                nkserver_ot:tag(?MODULE, <<"transcoder.cmd">>, Cmd),
                lager:error("NKLOG CMD ~p", [Cmd]),
                {ok, _} = nklib_exec:sync(Cmd, #{timeout=>Time}),
                nkserver_ot:log(?MODULE, <<"conversion finished">>),
                nkserver_ot:log(?MODULE, <<"writting file">>),
                {ok, File2} = file:read_file(<<BaseName/binary, ".png">>),
                nkserver_ot:log(?MODULE, <<"file wrote">>),
                nkserver_ot:finish(?MODULE),
                {ok, {<<"image/png">>, File2, #{}}}
            catch
                error:Error  ->
                    {error, {thumbnail_error, Error}}
            after
                file:delete(<<BaseName/binary, ".jpeg">>),
                file:delete(<<BaseName/binary, ".png">>),
                nkserver_ot:finish(?MODULE)
            end;
        {error, Error} ->
            {error, {thumbnail_error, Error}}
    end.





