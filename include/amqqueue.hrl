%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2018 Pivotal Software, Inc.  All rights reserved.
%%

-include("amqqueue_v1.hrl").
-include("amqqueue_v2.hrl").

-define(is_amqqueue(Q),
        (?is_amqqueue_v2(Q) orelse
         ?is_amqqueue_v1(Q))).

-define(amqqueue_is_auto_delete(Q),
        ((?is_amqqueue_v2(Q) andalso
          ?amqqueue_v2_field_auto_delete(Q) =:= true) orelse
         (?is_amqqueue_v1(Q) andalso
          ?amqqueue_v1_field_auto_delete(Q) =:= true))).

-define(amqqueue_is_durable(Q),
        ((?is_amqqueue_v2(Q) andalso
          ?amqqueue_v2_field_durable(Q) =:= true) orelse
         (?is_amqqueue_v1(Q) andalso
          ?amqqueue_v1_field_durable(Q) =:= true))).

-define(amqqueue_exclusive_owner_is(Q, Owner),
        ((?is_amqqueue_v2(Q) andalso
          ?amqqueue_v2_field_exclusive_owner(Q) =:= Owner) orelse
         (?is_amqqueue_v1(Q) andalso
          ?amqqueue_v1_field_exclusive_owner(Q) =:= Owner))).

-define(amqqueue_exclusive_owner_is_pid(Q),
        ((?is_amqqueue_v2(Q) andalso
          is_pid(?amqqueue_v2_field_exclusive_owner(Q))) orelse
         (?is_amqqueue_v1(Q) andalso
          is_pid(?amqqueue_v1_field_exclusive_owner(Q))))).

-define(amqqueue_state_is(Q, State),
        ((?is_amqqueue_v2(Q) andalso
          ?amqqueue_v2_field_state(Q) =:= State) orelse
         (?is_amqqueue_v1(Q) andalso
          ?amqqueue_v1_field_state(Q) =:= State))).

-define(amqqueue_v1_type, classic).
-define(amqqueue_type_is(Q, Type),
        ((?is_amqqueue_v2(Q) andalso
          ?amqqueue_v2_field_type(Q) =:= Type) orelse
         (?is_amqqueue_v1(Q) andalso
          ?amqqueue_v1_type =:= Type))).

-define(amqqueue_is_quorum(Q),
        (?is_amqqueue_v2(Q) andalso
         ?amqqueue_v2_field_type(Q) =:= quorum)).
