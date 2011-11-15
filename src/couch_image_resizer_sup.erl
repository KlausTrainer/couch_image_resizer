%% @doc Supervisor for the couch_image_resizer application.

-module(couch_image_resizer_sup).
-author("Klaus Trainer <klaus_trainer@posteo.de>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    WebSpecs = web_specs(),
    Processes = [WebSpecs],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs() ->
    Port = couch_image_resizer:get_app_env(port, 5985),
    Address = couch_image_resizer:get_app_env(address, "127.0.0.1"),
    WebConfig = [{ip, Address},
                 {port, Port},
                 {docroot, couch_image_resizer_deps:local_path(["priv", "www"])}],
    {couch_image_resizer_web,
     {couch_image_resizer_web, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
