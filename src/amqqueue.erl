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

-module(amqqueue). %% Could become amqqueue_v2 in the future.

-include_lib("rabbit_common/include/rabbit.hrl").

-export([new/9,
         field_vhost/0,
         get_args/1,
         get_decorators/1,
         get_exclusive_owner/1,
         get_gm_pids/1,
         get_leader/1,
         get_name/1,
         get_options/1,
         get_pid/1,
         get_policy_version/1,
         get_quorum_nodes/1,
         get_recoverable_slaves/1,
         get_slave_pids/1,
         get_state/1,
         get_sync_slave_pids/1,
         get_type/1,
         get_vhost/1,
         is_amqqueue/1,
         is_auto_delete/1,
         is_durable/1,
         pattern_match_all/0,
         pattern_match_on_name/1,
         reset_mirroring_and_decorators/1,
         set_gm_pids/2,
         set_immutable/1,
         set_pid/2,
         set_recoverable_slaves/2,
         set_state/2,
         macros/0]).

-define(record_version, amqqueue_v2).

new(Name,
    Pid,
    Durable,
    AutoDelete,
    Owner,
    Args,
    VHost,
    ActingUser,
    Type
   ) ->
    case quorum_queue_ff_enabled() of
        true ->
            #amqqueue{name               = Name,
                      durable            = Durable,
                      auto_delete        = AutoDelete,
                      arguments          = Args,
                      exclusive_owner    = Owner,
                      pid                = Pid,
                      slave_pids         = [],
                      sync_slave_pids    = [],
                      recoverable_slaves = [],
                      gm_pids            = [],
                      state              = live,
                      policy_version     = 0,
                      slave_pids_pending_shutdown = [],
                      vhost                       = VHost,
                      options = #{user => ActingUser},
                      type               = Type,
                      created_at         = erlang:monotonic_time()};
        false ->
            amqqueue_v1:new(
              Name,
              Pid,
              Durable,
              AutoDelete,
              Owner,
              Args,
              VHost,
              ActingUser)
    end.

is_amqqueue(#amqqueue{}) -> true;
is_amqqueue(Queue)       -> amqqueue_v1:is_amqqueue(Queue).

quorum_queue_ff_enabled() ->
    %% TODO: Check if the featurue flag is enabled or not.
    %%
    %% Possible solutions:
    %%   - We make a call to an Mnesia/ETS local table.
    %%   - We use the code_version moduel to "rebuild" this module on
    %%     the fly, so the function returns statically "true" or "false".
    true.

get_args(#amqqueue{arguments = Args}) -> Args;
get_args(Queue)                       -> amqqueue_v1:get_args(Queue).

get_decorators(#amqqueue{decorators = Decorators}) ->
    Decorators;
get_decorators(Queue) ->
    amqqueue_v1:get_decorators(Queue).

get_exclusive_owner(#amqqueue{exclusive_owner = Owner}) ->
    Owner;
get_exclusive_owner(Queue) ->
    amqqueue_v1:get_exclusive_owner(Queue).

get_gm_pids(#amqqueue{gm_pids = GMPids}) ->
    GMPids;
get_gm_pids(Queue) ->
    amqqueue_v1:get_gm_pids(Queue).

get_leader(#amqqueue{type = quorum, pid = {_, Leader}}) -> Leader.

get_name(#amqqueue{name = Name}) -> Name;
get_name(Queue)                  -> amqqueue_v1:get_name(Queue).

get_options(#amqqueue{options = Options}) -> Options;
get_options(Queue)                        -> amqqueue_v1:get_options(Queue).

get_pid(#amqqueue{pid = Pid}) -> Pid;
get_pid(Queue)                -> amqqueue_v1:get_pid(Queue).

set_pid(#amqqueue{} = Queue, Pid) ->
    Queue#amqqueue{pid = Pid};
set_pid(Queue, Pid) ->
    amqqueue_v1:set_pid(Queue, Pid).

get_policy_version(#amqqueue{policy_version = PV}) ->
    PV;
get_policy_version(Queue) ->
    amqqueue_v1:get_policy_version(Queue).

get_recoverable_slaves(#amqqueue{recoverable_slaves = Slaves}) ->
    Slaves;
get_recoverable_slaves(Queue) ->
    amqqueue_v1:get_recoverable_slaves(Queue).

set_recoverable_slaves(#amqqueue{} = Queue, Slaves) ->
    Queue#amqqueue{recoverable_slaves = Slaves};
set_recoverable_slaves(Queue, Slaves) ->
    amqqueue_v1:set_recoverable_slaves(Queue, Slaves).

%% New in v2.
get_quorum_nodes(#amqqueue{quorum_nodes = Nodes}) -> Nodes;
get_quorum_nodes(_)                               -> [].

get_slave_pids(#amqqueue{slave_pids = Slaves}) ->
    Slaves;
get_slave_pids(Queue) ->
    amqqueue_v1:get_slave_pids(Queue).

get_state(#amqqueue{state = State}) -> State;
get_state(Queue)                    -> amqqueue_v1:get_state(Queue).

set_state(#amqqueue{} = Queue, State) ->
    Queue#amqqueue{state = State};
set_state(Queue, State) ->
    amqqueue_v1:set_state(Queue, State).

get_sync_slave_pids(#amqqueue{sync_slave_pids = Pids}) ->
    Pids;
get_sync_slave_pids(Queue) ->
    amqqueue_v1:get_sync_slave_pids(Queue).

%% New in v2.
get_type(#amqqueue{type = Type}) -> Type;
get_type(_)                      -> ?amqqueue_v1_type.

get_vhost(#amqqueue{vhost = VHost}) -> VHost;
get_vhost(Queue)                    -> amqqueue_v1:get_vhost(Queue).

is_auto_delete(#amqqueue{auto_delete = AutoDelete}) ->
    AutoDelete;
is_auto_delete(Queue) ->
    amqqueue_v1:is_auto_delete(Queue).

is_durable(#amqqueue{durable = Durable}) -> Durable;
is_durable(Queue)                        -> amqqueue_v1:is_durable(Queue).

field_vhost() ->
    case quorum_queue_ff_enabled() of
        true  -> #amqqueue.vhost;
        false -> amqqueue_v1:field_vhost()
    end.

pattern_match_all() ->
    case quorum_queue_ff_enabled() of
        true  -> #amqqueue{_ = '_'};
        false -> amqqueue_v1:pattern_match_all()
    end.

pattern_match_on_name(Name) ->
    case quorum_queue_ff_enabled() of
        true  -> #amqqueue{name = Name, _ = '_'};
        false -> amqqueue_v1:pattern_match_on_name(Name)
    end.

reset_mirroring_and_decorators(#amqqueue{} = Queue) ->
    Queue#amqqueue{slave_pids      = [],
                   sync_slave_pids = [],
                   gm_pids         = [],
                   decorators      = undefined};
reset_mirroring_and_decorators(Queue) ->
    amqqueue_v1:reset_mirroring_and_decorators(Queue).

set_gm_pids(#amqqueue{} = Queue, GMPids) ->
    Queue#amqqueue{gm_pids = GMPids};
set_gm_pids(Queue, GMPids) ->
    amqqueue_v1:set_gm_pids(Queue, GMPids).

set_immutable(#amqqueue{} = Queue) ->
    Queue#amqqueue{pid                = none,
                   slave_pids         = none,
                   sync_slave_pids    = none,
                   recoverable_slaves = none,
                   gm_pids            = none,
                   policy             = none,
                   decorators         = none,
                   state              = none};
set_immutable(Queue) ->
    amqqueue_v1:set_immutable(Queue).

macros() ->
    io:format(
      "-define(is_~s(Q), is_record(Q, amqqueue, ~b)).~n~n",
      [?record_version, record_info(size, amqqueue)]),
    %% The field number starts at 2 because the first element is the
    %% record name.
    macros(record_info(fields, amqqueue), 2).

macros([Field | Rest], I) ->
    io:format(
      "-define(~s_field_~s(Q), element(~b, Q)).~n",
      [?record_version, Field, I]),
    macros(Rest, I + 1);
macros([], _) ->
    ok.
