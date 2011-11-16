%% @doc couch_image_resizer.

-module(couch_image_resizer).
-author("Klaus Trainer <klaus_trainer@posteo.de>").

-export([start/0, start/1, stop/0]).
-export([get_app_env/1, get_app_env/2]).

%% @spec start([ConfigPath::list()]) -> ok
%% @doc Start the couch_image_resizer server. `ConfigPath' specifies the
%%      location of the couch_image_resizer configuration file.
start([ConfigPath]) ->
    application:set_env(couch_image_resizer, configpath, ConfigPath),
    start().

%% @spec start() -> ok
%% @doc Start the couch_image_resizer server.
start() ->
    application:start(couch_image_resizer).

%% @spec stop() -> ok
%% @doc Stop the couch_image_resizer server.
stop() ->
    application:stop(couch_image_resizer).

%% @spec get_app_env(atom()) -> term()
%% @doc The official way to get the values set in couch_image_resizer's
%%      configuration file. Will return `undefined' if that option is unset.
get_app_env(Opt) ->
    get_app_env(Opt, undefined).

%% @spec get_app_env(atom(), term()) -> term()
%% @doc The official way to get the values set in couch_image_resizer's
%%      configuration file. Will return `Default' if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(couch_image_resizer, Opt) of
    {ok, Val} ->
        Val;
    _ ->
        case init:get_argument(Opt) of
        {ok, [[Val|_]]} -> Val;
        error -> Default
        end
    end.
