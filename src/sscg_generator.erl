% @doc Main application and CLI entry point.
-module(sscg_generator).

-behaviour(application).
-behaviour(cli).

-define(REQUIRED_MIN_OTP_VERSION, 26).

% Callbacks
-export([main/1]).
-export([cli/0]).

-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @doc Main CLI entry point.
main(Args) ->
    check_otp_version(?REQUIRED_MIN_OTP_VERSION),
    {ok, _} = application:ensure_all_started(sscg_generator),
    try
        cli:run(Args, #{
            progname => ?MODULE,
            modules => [?MODULE, 
                        sscg_generator_generate,
                        sscg_generator_publish],
            warn => false
        })
    catch
        Class:Reason:Stacktrace ->
            cli_abort(Class, Reason, Stacktrace)
    after
        application:stop(sscg_generator)
    end.

% @private
cli() ->
    #{
        arguments => [
            #{
                name => verbose,
                long => "-verbose",
                short => $v,
                action => count,
                type => boolean,
                help => "control verbosity level (max -vv)"
            }
        ]
    }.

% @doc Application start callback.
%
% Starts application and initialize data structures and tables.
start(_Type, _Args) ->
    {ok, self()}.

% @doc Application stop callback.
stop(_State) -> ok.

%--- Internal ------------------------------------------------------------------

check_otp_version(Version) ->
    check_otp_version(
        Version,
        list_to_integer(erlang:system_info(otp_release))
    ).

check_otp_version(Desired, Actual) when Actual < Desired ->
    sscg_generator_cli:abort("OTP version ~p too old. At least ~p required.", [
        Actual,
        Desired
    ]);
check_otp_version(_, _) ->
    ok.

cli_abort(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).
