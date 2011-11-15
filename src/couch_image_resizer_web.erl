%% @doc Web server for couch_image_resizer.

-module(couch_image_resizer_web).
-author("Klaus Trainer <klaus_trainer@posteo.de>").

-include("couch_image_resizer.hrl").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),

    Loop = fun(Req) ->
               try
                   ?MODULE:loop(Req, DocRoot)
               catch
               Type:What ->
                   Report = ["web request failed", {path, Req:get(raw_path)},
                             {type, Type}, {what, What},
                             {trace, erlang:get_stacktrace()}],
                   error_logger:error_report(Report),
                   Req:respond({500, [{"Content-Type", "text/plain; charset=utf-8"}], <<"{\"error\":\"error\",\"reason\":\"some error\"}">>})
               end
           end,

    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    RawPath = Req:get(raw_path),
    ContentType = {"Content-Type", "text/plain; charset=utf-8"},

    case Req:get(method) of
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        case string:tokens(RawPath, "/") of
        [_DbName, _DocId, _AttName] ->
            Scheme = ?a2l(Req:get(scheme)),
            Host = Req:get_header_value("host"),
            Url = Scheme ++ "://" ++ Host ++ RawPath,
            Method2 = ?l2a(string:to_lower(?a2l(Req:get(method)))),

            ReqHeaders = case Req:get_header_value("if-none-match") of
            undefined -> [];
            Value -> [{"If-None-Match", Value}]
            end,

            case ibrowse:send_req(Url, ReqHeaders, Method2) of
            {error, Reason} ->
                throw(Reason);
            {ok, "404", ResHeaders, Body} ->
                Req:respond({404, ResHeaders, Body});
            {ok, "304", ResHeaders, _} ->
                Req:respond({304, ResHeaders, <<>>});
            {ok, "200", ResHeaders, Att} ->
                case get_geometry(Req) of
                false ->
                    Req:respond({200, ResHeaders, Att});
                ambiguous ->
                    Req:respond({400, [ContentType], <<"{\"error\":\"bad_request\",\"reason\":\"resize query paramameter and x-imagemagick-resize header have different values\"}">>});
                invalid ->
                    Req:respond({400, [ContentType], <<"{\"error\":\"bad_request\",\"reason\":\"invalid geometry\"}">>});
                {ok, Size} ->
                    {"ETag", ETag0} = lists:keyfind("ETag", 1, ResHeaders),
                    Key = ?l2b(string:strip(ETag0, both, $") ++ "_" ++ Size),

                    case term_cache_ets:get(?IMAGE_CACHE, Key) of
                    not_found ->
                        TmpImgFile = make_temp_file_name(),
                        ScaledTmpImgFile = TmpImgFile ++ "_scaled",
                        file:write_file(TmpImgFile, Att),
                        FileType = os:cmd("file -b -i '" ++ TmpImgFile ++ "'"),

                        case string:substr(FileType, 1, 5) of
                        "image" ->
                            os:cmd("convert -resize '" ++ Size ++ "' '" ++ TmpImgFile ++ "' '" ++ ScaledTmpImgFile ++ "'"),
                            {ok, ScaledAtt} = file:read_file(ScaledTmpImgFile),
                            term_cache_ets:put(?IMAGE_CACHE, Key, ScaledAtt),
                            Req:respond({200, ResHeaders, ScaledAtt});
                        _ ->
                            Req:respond({400, [ContentType], <<"{\"error\":\"bad_request\",\"reason\":\"file is no image\"}">>})
                        end,

                        file:delete(TmpImgFile),
                        file:delete(ScaledTmpImgFile);
                    {ok, ScaledAtt} ->
                        Req:respond({200, ResHeaders, ScaledAtt})
                    end

                end
            end;
        _ ->
            Req:respond({404, [ContentType], <<"{\"error\":\"not_found\",\"reason\":\"missing\"}">>})
        end;
    _ ->
        Req:respond({405, [{"Allow", "GET,HEAD"} | [ContentType]], <<"{\"error\":\"method_not_allowed\",\"reason\":\"Only GET,HEAD allowed\"}">>})
    end.

%% Internal API

% @spec get_geometry(Req) -> false | ambiguous | invalid | {ok, string()}
get_geometry(Req) ->
    case lists:keyfind("resize", 1, Req:parse_qs()) of
    false ->
        case Req:get_header_value("x-imagemagick-resize") of
        undefined ->
            false;
        Size ->
            case is_valid_geometry(Size) of
            false -> invalid;
            true -> {ok, Size}
            end
        end;
    {"resize", Size} ->
        case Req:get_header_value("x-imagemagick-resize") of
        Value when Value =:= Size; Value =:= undefined ->
            case is_valid_geometry(Size) of
            false -> invalid;
            true -> {ok, Size}
            end;
        _ ->
            ambiguous
        end
    end.

% @doc c.f. http://www.imagemagick.org/script/command-line-processing.php#geometry
% @spec is_valid_geometry(string()) -> false | true
is_valid_geometry(Str) ->
    case re:run(Str, <<"^([\\d]+%?)?([x@]([\\d]*%?)?)?[\\^!><]?([+-][\\d]+([+-][\\d]+)?)?$">>, [{capture, none}]) of
    nomatch -> false;
    match -> true
    end.

%% @spec make_temp_file_name() -> string()
make_temp_file_name() ->
    {A, B, C} = now(),
    "/tmp/" ++ lists:flatten(io_lib:format("~p~p~p", [A, B, C])).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
