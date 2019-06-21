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
-module(nktranscoder_imagemagick_plugin).
-export([plugin_deps/0, plugin_config/3]).

plugin_deps() ->
    [nktranscoder].



plugin_config(_SrvId, Config, _Service) ->
    Syntax = #{
        max_conversion_time => {integer, 1000, none},
        '__defaults' => #{
            max_conversion_time => 120000
        }
    },
    nkserver_util:parse_config(Config, Syntax).