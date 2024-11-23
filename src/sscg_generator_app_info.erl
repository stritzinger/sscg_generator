-module(sscg_generator_app_info).

-export([get_app_name/0, get_version/0]).

%% @doc
%% Retrieves the name of the application by consulting the `.app.src` file.
-spec get_app_name() -> atom().
get_app_name() ->
    sscg_generator.

%% @doc
%% Retrieves the version of the application based on the application name.
-spec get_version() -> {ok, string()}.
get_version() ->
    application:get_key(get_app_name(), vsn).
