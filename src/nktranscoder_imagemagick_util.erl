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

-define(REQ_SPAN, ?MODULE).

thumbnail(SrvId, CT, File, Params) ->
    Format = maps:get(format, Params, <<>>),
    FormatOps1 = binary:split(Format, <<";">>, [global]),
    FormatOps2 = maps:from_list(FormatOps1),
    Syntax = #{
        size => integer
    },
    #{max_conversion_time:=Time} = nkserver:get_config(SrvId),
    lager:error("NKLOG CT ~p TIME: ~p", [CT, Time]),
    lager:error("NKLOG PARAMS: ~p", [Params]),
    SpanId = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?REQ_SPAN, SrvId, SpanId),
    nkserver_ot:log(?REQ_SPAN, <<"writing file">>),
    BaseName = <<"/tmp/trans-", (nklib_util:luid())/binary>>,
    try
        ok = file:write_file(<<BaseName/binary, ".jpeg">>, File),
        lager:error("NKLOG FILE ~p", [BaseName]),
        Cmd = <<
            "convert  ",
            BaseName/binary, ".jpeg ",
            BaseName/binary, ".png"
        >>,
        lager:error("NKLOG CMD ~p", [Cmd]),
        {ok, _} = nklib_exec:sync(Cmd, #{timeout=>Time}),
        nkserver_ot:log(?REQ_SPAN, <<"reading file">>),
        {ok, File2} = file:read_file(<<BaseName/binary, ".png">>),
        {ok, {CT, File2}}
    catch
        error:Error  ->
            {error, {thumbnail_error, Error}}
    after
        file:delete(<<BaseName/binary, ".jpeg">>),
        file:delete(<<BaseName/binary, ".png">>),
        nkserver_ot:finish(?REQ_SPAN)
    end.





