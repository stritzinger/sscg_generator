-module(sscg_generator_cli).
-moduledoc "This module provides helper functions for CLI".

% API
-export([init/1]).
-export([abort/2]).
-export([print/1]).
-export([print/2]).
-export([input/1]).
-export([confirm/1]).
-export([parse_authors/1]).
-export([serialize_args/2]).

% Callbacks
-export([format/2]).

-include_lib("kernel/include/logger.hrl").

%--- API -----------------------------------------------------------------------
-doc "Init client (e.g., adjusts log verbosity based on input arguments)".
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

-doc "Aborts the program with a red formatted error message".
-spec abort(Format :: string(), Args :: list()) -> Result :: no_return().
abort(Format, Args) ->
    io:format("~ts~n", [color:red(io_lib:format(Format, Args))]),
    erlang:halt(1).

-doc #{equiv => print(Text, [])}.
print(Text) -> print(Text, []).

-doc "Prints a formatted message to the console".
-spec print(string(), [term()]) -> ok.
print(Format, Args) ->
    io:format(Format ++ "~n", Args).

-doc "Prompts the user for input".
-spec input(string()) -> string() | no_return().
input(Prompt) ->
    case io:get_line(Prompt ++ " ") of
        eof ->
            "";
        {error, Reason} ->
            abort("Error reading input: ~p", [Reason]);
        Data ->
            string:trim(Data)
    end.

-doc "Prompts a yes/no-question and returns a boolean based on the response".
-spec confirm(string()) -> boolean().
confirm(Prompt) ->
    string:equal(input(Prompt), "yes", true).

-doc """
Parses a binary containing a comma-separated list of authors into a list of maps.
Each author entry should be in the format "<name>:<email>". Entries with missing
 name or email are also supported:
- "<name>:" will result in a map with `name` set and `email` as `undefined`.
- ":<email>" will result in a map with `email` set and `name` as `undefined`.
If the format is invalid, the function will abort with an error.
""".
-spec parse_authors(Authors) -> Result
    when Authors :: string(),
         Result  :: [#{name => binary(), email => binary()}] | no_return().
parse_authors(Authors) ->
    AuthorEntries = string:split(Authors, ",", all),
    lists:map(fun parse_author/1, AuthorEntries).

parse_author(AuthorEntry) ->
    case string:split(AuthorEntry, ":", all) of
        [Name, Email] ->
            Name1 = string:strip(Name),
            Email1 = string:strip(Email),
            case {string:equal(Name1, ""), string:equal(Email1, "")} of
                {false, false} ->
                    #{name  => unicode:characters_to_binary(Name1, utf8, utf8),
                      email => unicode:characters_to_binary(Email1, utf8, utf8)};
                {false, true} ->
                    #{name  => unicode:characters_to_binary(Name1, utf8, utf8),
                      email => undefined};
                {true, false} ->
                    #{name  => undefined,
                      email => unicode:characters_to_binary(Email1, utf8, utf8)};
                _ ->
                    abort("Failed parsing authors. Reason: ~ts.~n", [invalid_format])
            end;
        _ ->
            abort("Failed parsing authors. Reason: ~ts.~n", [invalid_format])
    end.

-doc """
Serializes the arguments map into a command-line binary using the Cli structure.
""".
-spec serialize_args(Args, Cli) -> Result
    when Args        :: map(), 
         Cli         :: argparse:command(),
         Result      :: binary().
serialize_args(Args, Cli) ->
    Arguments = maps:get(arguments, Cli),
    
    SerializedArgsList = lists:map(
        fun(ArgSpec) ->
            ArgName = maps:get(name, ArgSpec),
            serialize_arg(ArgName, Args, ArgSpec)
        end,
        Arguments),
    list_to_binary(string:join(SerializedArgsList, " ")).

serialize_arg(ArgName, Args, ArgSpec) ->
    case maps:find(ArgName, Args) of
        {ok, Value} ->
            LongOpt = maps:get(long, ArgSpec),
            case maps:get(type, ArgSpec) of
                binary ->
                    io_lib:format("-~ts ~ts", [LongOpt, binary_to_list(Value)]);
                {custom, _ParseFun} when ArgName == authors ->
                    io_lib:format("-~ts ~ts",
                                  [LongOpt,
                                   binary_to_list( serialize_authors(Value))]);
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
    unicode:characters_to_binary(string:join(AuthorEntries, ","), utf8, utf8).

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
    io_lib:format("~ts ~ts ~p:~p/~b~n", [
        Timestamp,
        format_level(Level),
        Module, Function, Arity
    ]).

format_level(Level) ->
    format_level(Level, string:uppercase(atom_to_binary(Level))).

format_level(debug, String) -> color:on_cyan([<<"[">>, String, <<"]">>]);
format_level(_Level, String) -> String.
