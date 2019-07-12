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
            size => integer, % Input File's size
            out_ext => binary, % Input File's extension
            '__defaults' => #{
                size => 100,
                out_ext => <<"png">>
            }
        }
    },

    case nklib_syntax:parse(Format, Syntax) of
        {ok, Parsed, _} ->
            % Get image from parsed tokens
            Image = maps:get(image, Parsed, #{}),
            % Get image from parsed tokens
            Size = maps:get(size, Image, 90),
            % Get input extension from parsed tokens
            InExt = getExtensionFromCT(CT),
            % Get image from parsed tokens
            OutExt = maps:get(out_ext, Image, <<"png">>),
            #{max_conversion_time:=Time} = nkserver:get_config(SrvId),
            nkserver_ot:log(?MODULE, "options parsed: ~p", [Parsed]),
            lager:notice("Starting thumbnail conversion (CT:~p, time:~p Size:~p, InExtension:~p, OutExtension: ~p", [CT, Time, Size, InExt, OutExt]),
            nkserver_ot:log(?MODULE, <<"writing file">>),
            BaseName = <<"/tmp/trans-", (nklib_util:luid())/binary>>,

            ok = file:write_file(<<BaseName/binary, ".", InExt/binary>>, File),
            nkserver_ot:log(?MODULE, <<"starting conversion">>),
            Cmd = list_to_binary([
                "convert ",
                BaseName, ".", InExt,
                " -auto-orient ",
                "-thumbnail 250x", integer_to_binary(Size), " ",
                "-unsharp 0x.5 ",
                BaseName, ".", OutExt
            ]),
            nkserver_ot:tag(?MODULE, <<"transcoder.cmd">>, Cmd),
            lager:notice("NKLOG CMD ~p", [Cmd]),
            {ok, _} = nklib_exec:sync(Cmd, #{timeout=>Time}),

            nkserver_ot:log(?MODULE, <<"conversion finished">>),
            nkserver_ot:log(?MODULE, <<"writting file">>),

            {ok, File2} = file:read_file(<<BaseName/binary, ".", OutExt/binary>>),

            nkserver_ot:log(?MODULE, <<"file written">>),

            file:delete(<<BaseName/binary, ".", InExt/binary>>),
            file:delete(<<BaseName/binary, ".", OutExt/binary>>),
        
            nkserver_ot:finish(?MODULE),
            {ok, {CT, File2, #{}}};

        {error, Error} ->
            {error, {thumbnail_error, Error}}
    end.

getExtensionFromCT(<<"image/png">>) ->
    <<"png">>;

getExtensionFromCT(<<"image/gif">>) ->
    <<"gif">>;

getExtensionFromCT(_) ->
    <<"png">>.