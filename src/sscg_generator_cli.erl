% @doc Helper functions for CLI.
-module(sscg_generator_cli).

% API
-export([init/1]).
-export([abort/2]).
-export([print/1]).
-export([print/2]).
-export([input/1]).
-export([confirm/1]).
-export([parse_range/2]).
-export([parse_authors/1]).
-export([serialize_args/3]).

% Callbacks
-export([format/2]).

-include_lib("kernel/include/logger.hrl").

-define(YES, "^[Yy]([Ee][Ss])?$").

%--- API -----------------------------------------------------------------------

init(Args) ->
    set_log_level(Args),
    ?LOG_DEBUG("ARGS: ~p", [Args]),
    ok.

set_log_level(#{verbose := Count}) when Count >= 2 ->
    logger:update_primary_config(#{level => debug});
set_log_level(#{verbose := 1}) ->
    logger:update_primary_config(#{level => info});
set_log_level(#{}) ->
    ok.

abort(Format, Args) ->
    io:format("~s~n", [color:red(io_lib:format(Format, Args))]),
    erlang:halt(1).

print(Text) -> print(Text, []).
print(Format, Args) ->
    io:format(Format ++ "~n", Args).

input(Prompt) ->
    case io:get_line(Prompt ++ " ") of
        eof ->
            "";
        {error, Reason} ->
            abort("Error reading input: ~p", [Reason]);
        Data ->
            re:replace(Data, "^[[:space:]]*+|[[:space:]]*+$", <<>>,
                [global, {return, binary}]
            )
    end.

confirm(Prompt) ->
    case re:run(input(Prompt), ?YES, [{capture, none}]) of
        match -> true;
        _     -> false
    end.

parse_range(Range, {DefaultFrom, DefaultTo}) ->
    {From, To} = case string:split(Range, "..") of
        [Start, []] ->
            {parse_date(Start, first), DefaultTo};
        [[], ToString] ->
            {DefaultFrom, parse_date(ToString, last)};
        [Start, ToString] ->
            {parse_date(Start, first), parse_date(ToString, last)};
        _ ->
            error(invalid_argument)
    end,
    case {From, To} of
        {From, To} when From > To -> error(invalid_argument);
        % {_, To} when To > DefaultTo -> error(invalid_argument);
        Else -> Else
    end.

-spec parse_authors(Authors) -> Result
    when Authors :: binary(),
         Result  :: [#{name => binary(), email => binary()}] | error.
parse_authors(Authors) ->
    AuthorEntries = string:split(Authors, ",", all),
    lists:map(fun parse_author/1, AuthorEntries).

parse_author(AuthorEntry) ->
    case string:split(AuthorEntry, ":", all) of
        [Name, Email] when Name =/= [] andalso Email =/= [] ->
            #{name  => list_to_binary(string:strip(Name)),
              email => list_to_binary(string:strip(Email))};
        [Name, []] -> 
            #{name  => list_to_binary(string:strip(Name)),
              email => undefined};
        [[], Email] -> 
            #{name  => undefined,
              email => list_to_binary(string:strip(Email))};
        _ -> 
            abort("Failed parsing authors. Reason: ~s. ~n",[invalid_format])
    end.

% @doc Serializes the Args map back into a command-line string using 
% the CLI structure.
-spec serialize_args(Args, CLI, CommandName) -> Result
  when Args        :: map(), 
       CLI         :: map(),
       CommandName :: string(),
       Result      :: binary().
serialize_args(Args, CLI, CommandName) ->
    Commands  = maps:get(commands, CLI),
    Command   = maps:get(CommandName, Commands),
    Arguments = maps:get(arguments, Command),
    
    SerializedArgsList = lists:map(
        fun(ArgSpec) ->
            ArgName = maps:get(name, ArgSpec),
            serialize_arg(ArgName, Args, ArgSpec)
        end,
        Arguments),
    list_to_binary(string:join(SerializedArgsList, " ")).

-spec serialize_arg(atom(), map(), map()) -> binary().
serialize_arg(ArgName, Args, ArgSpec) ->
    case maps:find(ArgName, Args) of
        {ok, Value} ->
            LongOpt = maps:get(long, ArgSpec),
            case maps:get(type, ArgSpec) of
                binary ->
                    io_lib:format("-~s ~s", [LongOpt, binary_to_list(Value)]);
                {custom, _ParseFun} when ArgName == authors ->
                    io_lib:format("-~s ~s", [LongOpt, binary_to_list( serialize_authors(Value))]);
                _ -> <<"">>
            end;
        error -> <<"">>  % If the argument is not present, return an empty string
    end.

-spec serialize_authors(Authors) -> Result
    when Authors ::[#{name := binary(), email := binary()}
                   | #{email := binary()}
                   | #{name  := binary()}],
         Result  :: binary().
serialize_authors(Authors) ->
    AuthorEntries = lists:map(fun serialize_author/1, Authors),
    list_to_binary(string:join(AuthorEntries, ",")).

serialize_author(Author) ->
    Name  = maps:get(name, Author, undefined),
    Email = maps:get(email, Author, undefined),
    NameStr = case Name of
                    undefined -> "";
                    _ when is_binary(Name) -> binary_to_list(Name);
                    _ -> Name
                end,
    EmailStr = case Email of
                    undefined -> "";
                    _ when is_binary(Email) -> binary_to_list(Email);
                    _ -> Email
                end,
    string:join([NameStr, EmailStr], ":").
%--- Callbacks -----------------------------------------------------------------

format(Event, _Config) ->
    #{
        level := Level,
        meta := #{
            mfa := {Module, Function, Arity},
            time := Time
        }
    } = Event,
    io:format("~p~n", [Event]),
    Timestamp = calendar:system_time_to_rfc3339(
        erlang:convert_time_unit(Time, microsecond, millisecond),
        [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}]
    ),
    io_lib:format("~s ~s ~p:~p/~b~n", [
        Timestamp,
        format_level(Level),
        Module, Function, Arity
    ]).

format_level(Level) ->
    format_level(Level, string:uppercase(atom_to_binary(Level))).

format_level(debug, String) -> color:on_cyan([<<"[">>, String, <<"]">>]);
format_level(_Level, String) -> String.

%----- Internal functions ------------------------------------------------------

parse_date(String, Preferred) ->
    Result = re:run(
        String,
        "
            (?<year>\\d{4}) # year
            (-(?<month>\\d{2}) # optional month
                (-(?<day>\\d{2}))? # optional day
            )?",
        [extended, {capture, all_names, list}]
    ),
    case Result of
        {match, Match} ->
            ToInteger = fun([]) -> undefined; (S) -> list_to_integer(S) end,
            fix_date(lists:map(ToInteger, Match), Preferred);
        nomatch ->
            error(invalid_argument)
    end.

fix_date([undefined, undefined, Year], first) ->
    {Year, 1, 1};
fix_date([undefined, undefined, Year], last) ->
    {Year, 12, 31};
fix_date([undefined, Month, Year], last) ->
    {Year, Month, calendar:last_day_of_the_month(Year, Month)};
fix_date([undefined, Month, Year], first) ->
    {Year, Month, 1};
fix_date([Day, Month, Year], _Preferred) ->
    Date = {Year, Month, Day},
    case calendar:valid_date(Date) of
        true -> Date;
        false -> error(invalid_argument)
    end.
