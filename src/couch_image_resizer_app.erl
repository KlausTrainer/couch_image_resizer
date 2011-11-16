%% @doc Callbacks for the couch_image_resizer application.

-module(couch_image_resizer_app).
-author("Klaus Trainer <klaus_trainer@posteo.de>").

-include("couch_image_resizer.hrl").

-behaviour(application).
-export([start/2, stop/1, read_config/0, read_config/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for couch_image_resizer.
start(_Type, _StartArgs) ->
    application:start(sasl),
    application:start(crypto),
    application:start(ibrowse),
    read_config(),
    CacheSize = couch_image_resizer:get_app_env(image_cache_size, "128MB"),
    term_cache_ets:start_link([{name, ?IMAGE_CACHE}, {size, CacheSize}]),
    couch_image_resizer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for couch_image_resizer.
stop(_State) ->
    ok.

%% @spec read_config() -> ok | {error, Reason}
%% @doc Read the couch_image_resizer erlenv configuration file and set
%%      environment variables.
read_config() ->
    read_config(couch_image_resizer:get_app_env(configpath, "config/couch_image_resizer.erlenv")).

%% @spec read_config(list()) -> ok | {error, Reason}
%% @doc Read the couch_image_resizer erlenv configuration file with filename
%%      `ConfigPath' and set environment variables.
read_config(ConfigPath) ->
    ConfigPairs =
        case file:consult(ConfigPath) of
        {ok, Terms} ->
            Terms;
        {error, Reason} ->
            error_logger:warning_msg("failed to read configuration from: ~p (~p)~n", [ConfigPath, Reason]),
            []
	    end,
    set_erlenv(ConfigPairs).

%% Internal API

set_erlenv([]) ->
    ok;
set_erlenv([{K, V}|T]) ->
    application:set_env(couch_image_resizer, K, V),
    error_logger:info_msg("set env variable ~p: ~p~n", [K, V]),
    set_erlenv(T).
