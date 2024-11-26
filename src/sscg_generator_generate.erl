% @doc Task to generate a .json from a SBOM and metadata.
-module(sscg_generator_generate).

% API
-export([cli/0, generate/1]).

% CycloneDX endpoint
-define(CYCLONEDX_BASE_URL, "http://cyclonedx.org/schema/").

%--- Types ---------------------------------------------------------------------

-type decoded_json() :: map(). 
-type file_path() :: binary(). 
-type folder_path() :: binary(). 
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
                        name => test,
                        long => "-test",
                        short => $t,
                        help => {"[-t <test_folder>]", 
                                 fun() -> "Test folder path" end},
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
                        default => <<"sscg.json">>,
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
-spec generate(Args) -> Result
  when Args :: #{sbom    := file_path(), 
                 test    := folder_path(),
                 authors := list()},
       Result :: ok | no_return().
generate(#{
    sbom    := SBOMFile,
    test    := TestFolder,  
    authors := Authors} = Args) ->

    Tests =
        case file:list_dir(TestFolder) of
            {ok, Files} ->
                lists:map(
                    fun(File) ->
                        FilePath = filename:join(TestFolder, File),
                        FileName = sscg_generator_utils:get_filename_from_path(FilePath),
                        case file:read_file(FilePath) of
                            {ok, Content} ->
                                {FileName, Content};
                            {error, Reason} ->
                                sscg_generator_cli:abort("Failed to read file ~s: ~p~n", [FilePath, Reason])
                        end
                    end, Files);
            {error, Reason} ->
                sscg_generator_cli:abort("Failed to list directory: ~p~n", [Reason])
        end,

    SBOMData =
        case sscg_generator_utils:read_json(SBOMFile) of
            {ok, JsonData} -> JsonData;
            {error, {file_not_available, NotAvailableReason}} ->
                sscg_generator_cli:abort(
                    "Error: Cannot read file ~s. Reason: ~p~n", 
                    [SBOMFile, NotAvailableReason]);
            {error, invalid_json} ->
                sscg_generator_cli:abort(
                    "Error: Invalid JSON format in ~s~n", 
                    [SBOMFile])
        end,

    case is_valid_sbom(SBOMData) of
        true -> ok;
        false -> 
            sscg_generator_cli:abort("Error: Invalid SBOM format. ~n", [])
    end,

    SpecVersion   = <<"1.6">>,
    Targets = case maps:get(<<"metadata">>, SBOMData, undefined) of
        #{<<"component">> := Component} ->
           [Component];
        _ -> []
    end,
    CommandName   = atom_to_list(?FUNCTION_NAME),
    Configuration = sscg_generator_cli:serialize_args(Args, cli(), CommandName),

    SSCGData = generate_sscg(
        #{spec_version  => SpecVersion, 
          authors       => Authors, 
          targets       => Targets,
          tests         => Tests,
          configuration => Configuration
        }),

    OutputPath = maps:get(output, Args),
    case sscg_generator_utils:write_json(OutputPath, SSCGData) of
        {ok, Path} -> 
            sscg_generator_cli:print(
                color:green(
                    io_lib:format(
                        "JSON successfully stored to ~s~n", [Path])));
        {error, {encode_errors, EncodingReason}} -> 
            sscg_generator_cli:abort(
                        "Failed to encode JSON. Reason: ~p~n", [EncodingReason]);
        {error, {write_failed, WriteReason}} -> 
            sscg_generator_cli:abort(
                        "Failed to store JSON. Reason: ~p~n", [WriteReason])
    end,
    ok.

%--- Internal Functions --------------------------------------------------------

%% @doc
%% Generates an SSCG (Static Software Supply Chain Guarantee) map in a CycloneDX
%% format using provided information. 
-spec generate_sscg(Map) -> Result
 when Map :: #{spec_version := binary(), 
               authors      := [binary()],
               targets       := [map()],
               tests        := [{ Name :: binary(), Content :: binary()}]},
      Result :: map().
generate_sscg(
    #{spec_version  := SpecVersion, 
      authors       := Authors,
      targets       := Targets ,
      tests         := Tests,
      configuration := Configuration
    }) ->

    Timestamp = sscg_generator_utils:current_timestamp(),
    SerialNumber = sscg_generator_utils:serial_number(),

    ReSCALEVersion = <<"1.0.0">>,
    ReSCALEStandardURL = <<"https://rescale-project.eu/standard/", ReSCALEVersion/binary>>,
    ReSCALEStandardConformanceURL= <<ReSCALEStandardURL/binary, "/conformance/complete">>,

    Claims = [{to_claim(Name), Content} || {Name, Content} <- Tests],

    #{
        bomFormat    => <<"CycloneDX">>,
        specVersion  => SpecVersion,
        serialNumber => SerialNumber,
        metadata     => #{
            timestamp => Timestamp,
            authors   => Authors,
            tools     => 
                #{components => 
                    [
                        generate_tool_info(sscg_generator, Configuration),
                        generate_tool_info(static_code_analysis_module,
                                           <<"RESCALE_STATIC_ANALYSIS_LANG=erlang\nRESCALE_DRY_RUN=false">> )
                    ]
                }
        },
        definitions  => #{
            standards => [
              #{
                'bom-ref'    => ReSCALEStandardURL,
                name         => <<"The ReSCALE Standard">>,
                description  => <<"The ReSCALE Standard describes a workflow to create a Trusted BOM (TBOM)">>,
                version      => ReSCALEVersion,
                requirements => [
                  #{
                    'bom-ref'  => ReSCALEStandardConformanceURL,
                    identifier => <<"/rescale/", ReSCALEVersion/binary ,"/conformance/complete">>,
                    title      => <<"Full conformance with ReSCALE's 'complete' profile, e.g. complete absence of findings">>
                  }
                ]
              }
            ]
        },
        declarations => #{
            targets        => #{components => Targets},
            attestations => [
                #{
                    assessor => <<"Producer Reference">>,
                    summary  => <<"Mapping of Requirements to Claims">>,
                    map      => [
                      #{
                        requirement   => ReSCALEStandardConformanceURL,
                        claims => [Name || {Name, _Content } <- Claims]
                      }
                    ]
                  }
            ],
            claims        => [
                #{
                    <<"bom-ref">> => Name,
                    %TODO: Include real target
                    target        => undefined,
                    evidence      => [to_evidence(Content)]
                } || {Name, Content} <- Claims
            ],
            evidence      => [
                #{
                    <<"bom-ref">> => to_evidence(Content),
                    description   => <<"TODO - Specify test results output">>,
                    data          => [
                        #{
                            contents => #{attachment => #{content => Content}}
                        }
                    ]
                } || {_Name, Content} <- Claims
            ]
        }
    }.


% @doc 
% Validates whether the given JSON data represents a valid SBOM (Software Bill 
% of Materials). A valid SBOM must meet the following criteria:
% - It must contain the `"specVersion"` field, which specifies the version of the SBOM.
% - It must include all required fields as defined by the schema for the specified version,
%   as outlined in the schema documentation at https://cyclonedx.org.
-spec is_valid_sbom(decoded_json()) -> boolean().
is_valid_sbom(JsonData) ->
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
%-spec validate_email(binary()) -> boolean().
% validate_email(Email) ->
%     Regex = "^[A-Za-z0-9._%+~\\-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
%     case re:run(Email, Regex, [unicode]) of
%         {match, _} -> true;
%         nomatch -> false
%     end.

-spec generate_tool_info(Tool, Configuration) -> Result 
 when Tool           :: sscg_generator | static_code_analysis_module,
      Configuration  :: binary(),
      Result         :: map().
generate_tool_info(sscg_generator, Configuration) -> 
    {ok, Version} = sscg_generator_app_info:get_version(),
    VersionBinary = list_to_binary(Version),

    Name = atom_to_binary(sscg_generator_app_info:get_app_name(), utf8),

    #{
        type           => application,
        name           => <<"ReSCALE SSCG Generator">>,
        version        => VersionBinary,
        description    => <<"A ReSCALE certified tool to generate SSCGs">>,
        purl           => <<"pkg:hex/", Name/binary, "@", VersionBinary/binary>>,
        data           => [
            #{
            name => <<"CLI configuration flags">>,
            type => configuration,
            contents => #{
                attachment => #{
                    content => Configuration
                    }
                }
            }
        ],
        % TODO: Generate it
        hashes => [#{alg     => <<"SHA-1">>, 
                     content => <<"2fd4e1c67a2d28fced849ee1bb76e7391b93eb12">>}]
    };
%% TODO: Dynamically generate this or extract to deps file
generate_tool_info(static_code_analysis_module = ToolName, Configuration) ->
    Version = <<"1.0.0">>,
    Name = atom_to_binary(ToolName, utf8),

    #{   
        type        => container,
        name        => <<"ReSCALE Static Code Analysis Module">>,
        version     => Version, 
        description => <<"A ReSCALE certified container to execute static testing">>,
        purl        => <<"pkg:docker/", Name/binary, "@", Version/binary>>,
        data        => [
            #{
            name     => <<"Docker Environment">>, 
            type     => configuration,
            contents => #{
                attachment => #{
                content => Configuration
                }
            }
            }
        ],
        % TODO: Generate it
        hashes => [#{alg     => <<"SHA-1">>, 
                     content => <<"35d1c8f259129dc800ec8e073bb68f995424619c">>}]
    }.

to_claim(Name) -> 
    list_to_binary(
        io_lib:format("Claim: Test Suite ~s found something!", [Name])).

to_evidence(Name) -> 
    list_to_binary(
        io_lib:format("Evidence: ~s.", [Name])).
