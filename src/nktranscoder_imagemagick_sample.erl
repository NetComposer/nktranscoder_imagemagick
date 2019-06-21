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
-module(nktranscoder_imagemagick_sample).
-export([test_inline_inline/0, test_http_inline/0]).

-define(ECHO_URL, "http://127.0.0.1:9001/transcoder/v1/thumbnail/imagemagick").



%% @doc
% File in http body, response in http response
test_inline_inline() ->
    Hds = [{<<"Content-Type">>, <<"ct1/ct2">>}],
    Body = <<"1234">>,
    {T1, {ok, 200, RepHds, Body}} = call_echo(#{}, Hds, Body),
    <<"ct1/ct2">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.



%% @doc
% File in external http, response in http response
test_http_inline() ->
    Params = #{
        format => <<"var1=1;var2=b">>,
        input_type => http,
        input_http_url => <<"https://kbob.github.io/images/sample-4.jpg">>
    },
    call_echo(Params, [], <<>>).





%% @private
call_echo(Params, Hds, Body) ->
    Params2 = [
        list_to_binary([
            nklib_util:to_binary(K),
            $=,
            http_uri:encode(nklib_util:to_binary(V))
        ])
        || {K, V} <- maps:to_list(Params)
    ],
    Params3 = nklib_util:bjoin(Params2, <<"&">>),
    Url = case Params3 of
        <<>> ->
            ?ECHO_URL;
        _ ->
            <<?ECHO_URL, "?", Params3/binary>>
    end,
    lager:notice("NKLOG URL ~s", [Url]),
    Opts = [
        with_body,
        {recv_timeout, 180000}
    ],
    Start = nklib_date:epoch(usecs),
    R = hackney:request(post, Url, Hds, Body, Opts),
    Time = nklib_date:epoch(usecs) - Start,
    {Time, R}.
