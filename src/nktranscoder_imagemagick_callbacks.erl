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

-module(nktranscoder_imagemagick_callbacks).
-export([msg/1, transcoder_operation/5]).
-include("nktranscoder_imagemagick.hrl").


%% ===================================================================
%% Msg Callbacks
%% ===================================================================

msg(_)   		                    -> continue.



transcoder_operation(SrvId, [<<"thumbnail">>, <<"imagemagick">>], Params, CT, File) ->
    nktranscoder_imagemagick_util:thumbnail(SrvId, CT, File, Params);

transcoder_operation(_, _, _Params, _CT, _File) ->
    continue.
