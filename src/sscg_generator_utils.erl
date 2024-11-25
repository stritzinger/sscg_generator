-module(sscg_generator_utils).
-moduledoc """
Helper functions for the SSCG generator escript, including utilities for working
 with timestamps, UUIDs, reading and writing JSON files, and handling file paths.
""".

% API
-export([current_timestamp/0,
         serial_number/0]).

-export([read_json/1,
         write_json/2,
         write_json/3,
         get_filename_from_path/1,
         process_file/1]).

% Includes
-include("sscg_generator.hrl").

%--- API ---------------------------------------------------------------------

%--- API: Types

-doc """
Generates the current UTC timestamp in ISO 8601 format 
(e.g., \"2024-09-18T23:45:52Z\")
""".
-spec current_timestamp() -> binary().
current_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp =io_lib:format("~4..0B-~2..0B-~2..0B~ts~2..0B:~2..0B:~2..0BZ",
                             [Year, Month, Day, "T", Hour, Minute, Second]),
    iolist_to_binary(Timestamp).

-doc "Generates a unique serial number based on a UUID".
-spec serial_number() -> binary().
serial_number() ->
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    <<"urn:uuid:", UUID/binary>>.

%--- API: Files

-doc "Reads the content of a JSON file from the given path".
-spec read_json(JsonPath) -> Result
    when JsonPath :: file_path(),
         Result   :: {ok, decoded_json()} 
                     | {error, invalid_json} 
                     | {error, {file_not_available, Reason :: term()}}.
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

-doc "Writes the given JSON data to the specified file path".
-spec write_json(OutputPath, JsonData) -> Result
    when OutputPath    :: file_path(), 
         JsonData      :: jsone:json_value(),
         Result        :: {ok, file_path()} 
                          | {error, {encoding_failed, term()}} 
                          | {error, {write_failed, term()}}
                          | {error, {encode_error, term()}}.
write_json(OutputPath, JsonData) -> 
    DefaultOptions = [{indent, 4},
                      {float_format, [{scientific, 2}]}, 
                      skip_undefined, native_forward_slash],
    write_json(OutputPath, JsonData, DefaultOptions).

-doc "Writes the given JSON data to the specified file path".
-spec write_json(OutputPath, JsonData, EncodeOptions) -> Result
    when OutputPath    :: file_path(), 
         JsonData      :: jsone:json_value(),
         EncodeOptions :: [jsone:encode_option()],
         Result        :: {ok, file_path()} 
                          | {error, {encode_error, term()}} 
                          | {error, {write_failed, term()}}
                          | {error, {encode_error, term()}}.
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
            {error, {encode_error, EncodingReason}}
    end.

-doc "Extracts the file name from a full file path".
-spec get_filename_from_path(FilePath) -> FileName 
    when FilePath :: file_path(),
         FileName :: binary().
get_filename_from_path(FilePath) ->
    filename:basename(FilePath).

-doc "Processes a file by reading its content and extracting its file name".
-spec process_file(FilePath) -> Result
    when FilePath :: file_path(),
         Result   :: {ok, {FileName :: binary(), Content :: binary()}} 
                     | {error, Reason :: term()}.
process_file(FilePath) ->
    FileName = sscg_generator_utils:get_filename_from_path(FilePath),
    case file:read_file(FilePath) of
        {ok, Content}   -> {ok, {FileName, Content}};
        {error, Reason} -> {error, Reason}
    end.