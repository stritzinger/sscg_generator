-module(sscg_generator).
-moduledoc "Main application and CLI entry point".

-behaviour(application).
-behaviour(cli).

-define(REQUIRED_OTP_VERSION, 26).

% Callbacks
-export([main/1, cli/0]).
-export([start/2, stop/1]).

%--- Callbacks -----------------------------------------------------------------

%--- Callbacks: Application
-doc "Main CLI entry point".
-spec main(Args :: [string()]) -> term() | no_return().
main(Args) ->
    case ensure_minimum_otp_version(?REQUIRED_OTP_VERSION) of
        ok ->
            {ok, _} = application:ensure_all_started(sscg_generator),
            Options = #{progname => ?MODULE,
                        modules  => [?MODULE,
                                     sscg_generator_generate,
                                     sscg_generator_publish],
                        warn     => suppress},
            cli:run(Args, Options);
        {error, {otp_version_too_old, Actual, Desired}} ->
            sscg_generator_cli:abort("OTP version ~p is too old. At least ~p required.~n", [Actual, Desired]);
        {error, {unable_to_determine_otp_version, Info}} ->
            sscg_generator_cli:abort("Error: Unable to determine OTP version. Info: ~p~n", [Info])
   end.

-doc "Defines the CLI structure for the main command".
-spec cli() -> args:command().
cli() ->#{arguments => [
              #{name => verbose,
                long => "-verbose",
                short => $v,
                action => count,
                type => boolean,
                help => "control verbosity level (max -vv)"}]}.

%--- Callbacks: Cli
-doc "Application start callback".
start(_Type, _Args) ->
    {ok, self()}.

-doc "Application stop callback".
stop(_State) -> ok.

%--- Internal ------------------------------------------------------------------
ensure_minimum_otp_version(RequiredVersion) ->
    Release = erlang:system_info(otp_release),
    try
        compare_otp_versions(RequiredVersion, list_to_integer(Release))
    catch
        _:_ -> {error, {unable_to_determine_otp_version, Release}}
    end.

compare_otp_versions(Required, Current) when Current < Required ->
    {error, {otp_version_too_old, Current, Required}};
compare_otp_versions(_, _) ->
    ok.