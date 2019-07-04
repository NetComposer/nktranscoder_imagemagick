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
-export([test_http_inline/0, test_inline_inline/0]).


%% @doc
% File in external http, response in http response
test_http_inline() ->
    Params = #{
        format => <<"image;size=10000">>,
        input_type => http,
        input_http_url => <<"https://kbob.github.io/images/sample-4.jpg">>
    },
    call_thumbnail(Params, [], <<>>).


test_inline_inline() ->
    {ok, 200, _, Ref} = hackney:request(<<"https://kbob.github.io/images/sample-4.jpg">>),
    {ok, Body} = hackney:body(Ref),
    Hds = [{<<"Content-Type">>, <<"image/jpg">>}],
    {T1, {ok, 200, RepHds, Body}} = nktranscoder_sample:call(<<"thumbnail">>, #{}, Hds, Body),
    <<"image/jpeg">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.


%% @private
call_thumbnail(Params, Hds, Body) ->
    nktranscoder_sample:call(<<"thumbnail/imagemagick">>, Params, Hds, Body).

