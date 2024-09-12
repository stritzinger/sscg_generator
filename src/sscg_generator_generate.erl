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


-type json() :: map(). 
-type file_path() :: binary(). 
-type spec_version() :: <<_ : _*8>>.  % A version-like format, i.e., "1.0", "2.0"
%--- API -----------------------------------------------------------------------

% @doc Defines the CLI structure for the 'generate' command.
-spec cli() -> map().
cli() ->
    #{
        commands => #{
            "generate" => #{
                help => "Generate a SSCG using a SBOM and metadata files",
                arguments => [
                    #{
                        name => sbom,
                        long => "-sbom",
                        short => $s,
                        help => {"[-s <SBOM_file>]", fun() -> "SBOM JSON file path" end},
                        type => binary,
                        required => true
                    },
                    #{
                        name => output,
                        long => "-output",
                        short => $o,
                        help => {"[-p <output_path>]", fun() -> "Output file path and name" end},
                        type => binary,
                        default => <<"./priv/output/sscg_output.json">>,
                        required => false
                    }
                ]
            }
        }
    }.

% @doc The main task to generate a JSON SSCG from a SBOM file and metadata.
-spec generate(Args :: #{sbom := file_path()}) -> ok | no_return().
generate(#{sbom := SBOM_File} = Args) ->

    JsonData =
        case read_json_file(SBOM_File) of
            {ok, Data} -> Data;
            {error, {file_not_available, Reason}} ->
                sscg_generator_cli:abort("Error: Cannot read file ~s. Reason: ~p~n", [SBOM_File, Reason]);
            {error, invalid_json} ->
                sscg_generator_cli:abort("Error: Invalid JSON format in ~s~n", [SBOM_File])
        end,

    case is_a_valid_sbom(JsonData) of
        true -> ok;
        false -> 
            sscg_generator_cli:abort("Error: Invalid SBOM format. ~n", [])
    end,

    SpecVersion = maps:get(<<"specVersion">>, JsonData),

    _Schema = 
        case get_schema(SpecVersion) of 
            {ok, SchemaData} -> 
                SchemaData;
            {error, FetchReason} -> 
                sscg_generator_cli:abort("Impossible to retrieve the schema from https://cyclonedx.org. Reason: ~p~n", [FetchReason])
        end,
    
    SSCGData = generate_sscg(SpecVersion),

    EncodeOptions = [{indent, 4}, {float_format, [{scientific, 2}]}, skip_undefined],
    Json = jsone:encode(SSCGData, EncodeOptions),

    OutputPath = maps:get(output, Args),
    case file:write_file(OutputPath, Json) of
        ok -> 
            sscg_generator_cli:print(
                color:green(io_lib:format("JSON successfully stored to ~s~n", [OutputPath]))
            );
        {error, FailedReason} -> 
            sscg_generator_cli:abort(
                color:red(io_lib:format("Failed to store JSON. Reason: ~p~n", [FailedReason]))
            )
    end,

    ok.

%--- Internal Functions --------------------------------------------------------

generate_sscg(SpecVersion) ->
    Timestamp = sscg_generator_utils:current_timestamp(),
    SerialNumber = sscg_generator_utils:serial_number(),
    Email = sscg_generator_utils:prompt_for(<<"author's email">>, 
                                            fun validate_email/1, 
                                            <<"Invalid email format. Please use the format: letters@domain.extension (e.g., name@example.com)">>),
    Name  = sscg_generator_utils:prompt_for(<<"author's name">>),

    #{
        bomFormat => <<"CycloneDX">>,
        specVersion => SpecVersion,
        serialNumber => SerialNumber,
        metadata => #{
            timestamp => Timestamp,
            authors => [
                #{
                    name => Name,
                    email => Email
                }
            ]
        }
    }.

% @doc Reads the content of a JSON file from the given path.
-spec read_json_file(JsonPath) -> Result
      when JsonPath :: file_path(),
           Result   :: {ok, json()} 
                       | {error, invalid_json} 
                       | {error, {file_not_found, Reason :: term()}}.
read_json_file(JsonPath) ->
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


% @doc 
% Validates whether the given JSON data represents a valid SBOM (Software Bill of Materials).
% A valid SBOM must include:
% - `"bomFormat"`: Specifies the format of the SBOM, which should be `"CycloneDX"`.
% - `"specVersion"`: The specification version of the SBOM.
% - `"$schema"`: The URL of the schema, which is validated against the expected schema URL.
-spec is_a_valid_sbom(json()) -> boolean().
is_a_valid_sbom(JsonData) ->
    case {maps:find(<<"bomFormat">>, JsonData),
            maps:find(<<"specVersion">>, JsonData),
            maps:find(<<"$schema">>, JsonData)} of
        {{ok, BomFormat}, {ok, SpecVersion}, {ok, SchemaURL}} ->
            URL =  <<?CYCLONEDX_BASE_URL, "bom-", SpecVersion/binary, ".schema.json">>,
            (URL =:= SchemaURL) andalso (BomFormat =:= <<"CycloneDX">>);
        _ ->
            false
    end.

% @doc Retrieves the CycloneDX schema for the given specification version.
-spec get_schema(SpecVersion) -> Result
      when SpecVersion :: spec_version(),
           Result :: {ok, binary()} | {error, term()}.
get_schema(SpecVersion) ->
    URL =  <<?CYCLONEDX_BASE_URL, "bom-", SpecVersion/binary, ".schema.json">>,
    sscg_generator_http:get_json(URL).

% @doc Function to validate an email address
-spec validate_email(binary()) -> boolean().
validate_email(Email) ->
    Regex = "^[A-Za-z0-9._%+~\\-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
    case re:run(Email, Regex, [unicode]) of
        {match, _} -> true;
        nomatch -> false
    end.