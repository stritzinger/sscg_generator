% @doc Task to generate a .json from a SBOM and metadata.
-module(sscg_generator_generate).

% API
-export([cli/0, generate/1]).

-ifdef(TEST).
-export([group_by/3]).
-endif.

% Includes
-include_lib("kernel/include/logger.hrl").

% CycloneDX endpoint
-define(CYCLONEDX_BASE_URL, "http://cyclonedx.org/schema/").

%--- Types ---------------------------------------------------------------------


-type decoded_json() :: map(). 
-type file_path() :: binary(). 
-type spec_version() :: <<_ : _*8>>.  % A version-like format, i.e., "1.0", "2.0"
%--- API -----------------------------------------------------------------------

% @doc Defines the CLI structure for the 'generate' command.
-spec cli() -> map().
cli() ->
    DefaultName  = undefined,
    DefaultEmail = undefined,
    ParseAuthors = fun(A) -> sscg_generator_cli:parse_authors(A, {DefaultName, DefaultEmail}) end,
    #{
        commands => #{
            "generate" => #{
                help => "Generate a SSCG using a SBOM and metadata files",
                arguments => [
                    #{
                        name => sbom,
                        long => "-sbom",
                        short => $s,
                        help => {"[-s <SBOM_file>]", 
                                 fun() -> "SBOM JSON file path" end},
                        type => binary,
                        required => true
                    },
                    #{
                        name => output,
                        long => "-output",
                        short => $o,
                        help => {"[-p <output_path>]", 
                                 fun() -> "Output file path and name" end},
                        type => binary,
                        default => <<"./priv/output/sscg_output.json">>,
                        required => false
                    },
                    #{
                        name => authors,
                        long => "-authors",
                        short => $a,
                        help => {"[-a <name1>:<email1>,<name2>:<email2>,...]", 
                                 fun() -> "Specify authors' names and emails in the format: name:email" end},
                        type => {custom, ParseAuthors},
                        default => "",
                        required => false
                    }
                ]
            }
        }
    }.

% @doc The main task to generate a JSON SSCG from a SBOM file and metadata.
-spec generate(Args :: #{sbom := file_path(), authors := list()}) -> ok | no_return().
generate(#{sbom := SBOM_File, authors := Authors} = Args) ->

    JsonData =
        case sscg_generator_utils:read_json(SBOM_File) of
            {ok, Data} -> Data;
            {error, {file_not_available, Reason}} ->
                sscg_generator_cli:abort(
                    "Error: Cannot read file ~s. Reason: ~p~n", 
                    [SBOM_File, Reason]);
            {error, invalid_json} ->
                sscg_generator_cli:abort(
                    "Error: Invalid JSON format in ~s~n", 
                    [SBOM_File])
        end,

    case is_a_valid_sbom(JsonData) of
        true -> ok;
        false -> 
            sscg_generator_cli:abort("Error: Invalid SBOM format. ~n", [])
    end,

    SpecVersion = maps:get(<<"specVersion">>, JsonData),
    SSCGData = generate_sscg(#{spec_version => SpecVersion, authors => Authors}),

    OutputPath = maps:get(output, Args),
    case sscg_generator_utils:write_json(OutputPath, SSCGData) of
        {ok, Path} -> 
            sscg_generator_cli:print(
                color:green(
                    io_lib:format(
                        "JSON successfully stored to ~s~n", [Path])));
        {error, {encoding_failed, EncodingReason}} -> 
            sscg_generator_cli:abort(
                    io_lib:format(
                        "Failed to encode JSON. Reason: ~p~n", [EncodingReason]));
        {error, {write_failed, WriteReason}} -> 
            sscg_generator_cli:abort(
                    io_lib:format(
                        "Failed to store JSON. Reason: ~p~n", [WriteReason]))
    end,
    ok.

%--- Internal Functions --------------------------------------------------------

generate_sscg(#{spec_version := SpecVersion, authors := Authors}) ->
    Timestamp = sscg_generator_utils:current_timestamp(),
    SerialNumber = sscg_generator_utils:serial_number(),


    #{
        bomFormat => <<"CycloneDX">>,
        specVersion => SpecVersion,
        serialNumber => SerialNumber,
        metadata => #{
            timestamp => Timestamp,
            authors => Authors
        }
    }.


% @doc 
% Validates whether the given JSON data represents a valid SBOM (Software Bill 
% of Materials). A valid SBOM must meet the following criteria:
% - It must contain the `"specVersion"` field, which specifies the version of the SBOM.
% - It must include all required fields as defined by the schema for the specified version,
%   as outlined in the schema documentation at https://cyclonedx.org.
-spec is_a_valid_sbom(decoded_json()) -> boolean().
is_a_valid_sbom(JsonData) ->
    case maps:find(<<"specVersion">>, JsonData) of
        {ok, SpecVersion} ->
            SchemaJson = 
                case get_schema(SpecVersion) of 
                    {ok, SchemaData} -> 
                        SchemaData;
                    {error, FetchReason} -> 
                        sscg_generator_cli:abort(
                            "Impossible to retrieve the schema from https://cyclonedx.org."
                            " Reason: ~p~n", [FetchReason])
                end,
            RequiredFields = maps:get(<<"required">>, SchemaJson, []),
            lists:all(
                fun(Field) -> maps:is_key(Field, JsonData) end, 
                RequiredFields);
        _ ->
            false
    end.

% @doc Retrieves the CycloneDX schema for the given specification version.
-spec get_schema(SpecVersion) -> Result
      when SpecVersion :: spec_version(),
           Result      :: {ok, decoded_json()} 
                          | {error, {request_failed,    Reason}}
                          | {error, {unexpected_status, StatusCode}}
                          | {error, {body_read_error,   Reason}}
                          | {error, {invalid_json,      Reason}},
           Reason      :: term(),
           StatusCode  :: non_neg_integer().
get_schema(SpecVersion) ->
    URL  =  <<?CYCLONEDX_BASE_URL, "bom-", SpecVersion/binary, ".schema.json">>,
    sscg_generator_http:get_json(URL).

% @doc Function to validate an email address
-spec validate_email(binary()) -> boolean().
validate_email(Email) ->
    Regex = "^[A-Za-z0-9._%+~\\-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
    case re:run(Email, Regex, [unicode]) of
        {match, _} -> true;
        nomatch -> false
    end.
