% @doc Helper functions for the escript.
-module(sscg_generator_utils).

-export([current_timestamp/0]).
-export([serial_number/0, uuid/0]).
-export([read_json/1, write_json/2, write_json/3]).

-type file_path()    :: binary().
-type decoded_json() :: map().

current_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp =io_lib:format("~4..0B-~2..0B-~2..0B~ts~2..0B:~2..0B:~2..0BZ",
                             [Year, Month, Day, "T", Hour, Minute, Second]),
    iolist_to_binary(Timestamp).

uuid() -> uuid:uuid_to_string(uuid:get_v4(), binary_standard).

serial_number() ->
    UUID = uuid(),
    <<"urn:uuid-", UUID/binary>>.

% @doc Reads the content of a JSON file from the given path.
-spec read_json(JsonPath) -> Result
      when JsonPath :: file_path(),
           Result   :: {ok, decoded_json()} 
                       | {error, invalid_json} 
                       | {error, {file_not_found, Reason :: term()}}.
read_json(JsonPath) ->
    case file:read_file(JsonPath) of
        {ok, Binary} -> 
            try
                {ok, jsone:decode(Binary)}
            catch
                _:_ -> 
                    {error, invalid_json}
            end;
        {error, Reason} ->
            {error, {file_not_available, Reason}}
    end.

% @doc Writes the given JSON data to the specified file path.
-spec write_json(OutputPath, JsonData) -> Result
    when OutputPath    :: file_path(), 
         JsonData      :: binary(),
         Result        :: {ok, file_path()} 
                          | {error, {encoding_failed, term()}} 
                          | {error, {write_failed, term()}}.
write_json(OutputPath, JsonData) -> 
    DefaultOptions = [{indent, 4},
                      {float_format, [{scientific, 2}]}, 
                      skip_undefined],
    write_json(OutputPath, JsonData, DefaultOptions).

% @doc Writes the given JSON data to the specified file path.
-spec write_json(OutputPath, JsonData, EncodeOptions) -> Result
    when OutputPath    :: file_path(), 
         JsonData      :: binary(),
         EncodeOptions :: [jsone:encode_option()],
         Result        :: {ok, file_path()} 
                          | {error, {encoding_failed, term()}} 
                          | {error, {write_failed, term()}}.
write_json(OutputPath, 
           JsonData, 
           EncodeOptions) ->
    try 
        Json = jsone:encode(JsonData, EncodeOptions),
        case file:write_file(OutputPath, Json) of
            ok -> 
                {ok, OutputPath};
            {error, FailedReason} ->
                {error, {write_failed, FailedReason}}
        end
    catch
        _:{error, EncodingReason} ->
            {error, {encoding_failed, EncodingReason}}
    end.