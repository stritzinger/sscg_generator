% @doc Task to generate a .json from a SBOM and metadata.
-module(sscg_generator_generate).

% API
-export([cli/0, generate/1]).

-ifdef(TEST).
-export([group_by/3]).
-endif.

%Includes
-include_lib("stdlib/include/zip.hrl").
-include_lib("sscg_generator_types.hrl").

% CycloneDX endpoint
-define(CYCLONEDX_BASE_URL, "http://cyclonedx.org/schema/").

%--- API -----------------------------------------------------------------------

% @doc Defines the CLI structure for the 'generate' command.
-spec cli() -> map().
cli() ->
    DefaultName  = undefined,
    DefaultEmail = undefined,
    ParseAuthors = 
        fun(Authors) -> 
            sscg_generator_cli:parse_authors(Authors, {DefaultName, DefaultEmail})
        end,
    #{commands => 
          #{"generate" => 
                #{help      => "Generate a SSCG using a SBOM and metadata files",
                  arguments => 
                      [#{name      => sbom,
                          long      => "-sbom",
                          short     => $s,
                          help      => {"[-s <SBOM_file>]", 
                                      fun() -> "SBOM JSON file path" end},
                          type     => binary,
                          required => true},
                       #{name     => test,
                         long     => "-test",
                         short    => $t,
                         help     => {"[-t <test_folder>]", 
                                      fun() -> "Test folder path" end},
                         type     => binary,
                         required => true},
                       #{name     => output,
                         long     => "-output",
                         short    => $o,
                         help     => {"[-p <output_path>]", 
                                      fun() -> "Output file path and name" end},
                         type     => binary,
                         default  => <<"sscg.json">>,
                         required => false},
                       #{name     => authors,
                         long     => "-authors",
                         short    => $a,
                         help     => {"[-a <name1>:<email1>,<name2>:<email2>,...]", 
                                      fun() -> 
                                          "Specify authors' names and emails in the format: name:email" 
                                      end},
                         type     => {custom, ParseAuthors},
                         default  => "",
                         required => false}]
                }
            }
        }.

% @doc The main task to generate a JSON SSCG from a SBOM file and metadata.
-spec generate(Args) -> Result
  when Args :: #{sbom    := file_path(), 
                 test    := folder_path(),
                 authors := list()},
       Result :: ok | no_return().
generate(#{sbom    := SBOMFile,
           test    := TestFolder,  
           authors := Authors} = Args) ->
    TestData = 
        case file:list_dir(TestFolder) of
            {ok, Files} ->
                sscg_generator_cli:print("Files ~p", [Files]),
                lists:flatmap(fun (File) -> 
                    FilePath = filename:join(TestFolder, File),
                    FileName = sscg_generator_utils:get_filename_from_path(FilePath),
                    case sscg_generator_utils:get_file_format(FileName) of
                        zip -> extract_zip_file(FilePath);
                        csv -> [prepare_csv_file(FilePath, FileName)];
                        _   -> [prepare_regular_file(FilePath, FileName)]
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
        false -> sscg_generator_cli:abort("Error: Invalid SBOM format. ~n", [])
    end,

    SpecVersion   = <<"1.6">>,
    Targets = case maps:get(<<"metadata">>, SBOMData, undefined) of
                  #{<<"component">> := Component} -> [Component];
                  _ -> []
              end,

    CommandName   = atom_to_list(?FUNCTION_NAME),
    Configuration = sscg_generator_cli:serialize_args(Args, cli(), CommandName),

    SSCGData = generate_sscg(
        #{spec_version  => SpecVersion, 
          authors       => Authors, 
          targets       => Targets,
          tests         => TestData,
          configuration => Configuration}),

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

%--- Internal Fun.: SSCG generation

% Generates an SSCG (Static Software Supply Chain Guarantee) map in a CycloneDX
% format using provided information. 
-spec generate_sscg(Map) -> Result
 when Map :: #{spec_version := binary(), 
               authors      := [binary()],
               targets      := [map()],
               tests        := [#{name    => binary(), 
                                  content => binary(), 
                                  format  => codebert | undefined}]},
      Result :: map().
generate_sscg(
    #{spec_version  := SpecVersion, 
      authors       := Authors,
      targets       := Targets ,
      tests         := Tests,
      configuration := Config
    }) ->

    Timestamp    = sscg_generator_utils:current_timestamp(),
    SerialNumber = sscg_generator_utils:serial_number(),

    ReSCALEVersion     = <<"1.0.0">>,
    ReSCALEStandardURL = <<"https://rescale-project.eu/standard/", ReSCALEVersion/binary>>,
    ReSCALEStandardConformanceURL= <<ReSCALEStandardURL/binary, "/conformance/complete">>,

    #{claims := Claims, evidence := Evidences} = 
        lists:foldl(fun(Test, #{claims := AccC, evidence := AccE}) ->
                        #{claims := NewCs, evidence := NewEs} = process_test(Test),
                        #{claims   => AccC ++ NewCs, 
                          evidence => AccE ++ NewEs}
                    end,
                    #{claims => [], evidence => []}, 
                    Tests),

    #{bomFormat    => <<"CycloneDX">>,
      specVersion  => SpecVersion,
      serialNumber => SerialNumber,
      metadata     => #{timestamp => Timestamp,
                        authors   => Authors,
                        tools     => 
                            #{components => [generate_tool_info(sscg_generator, Config),
                                             generate_tool_info(static_code_analysis_module,
                                           <<"RESCALE_STATIC_ANALYSIS_LANG=erlang\nRESCALE_DRY_RUN=false">>)]}},
      definitions  =>
          #{standards =>
              [#{'bom-ref'     => ReSCALEStandardURL,
                  name         => <<"The ReSCALE Standard">>,
                  description  => <<"The ReSCALE Standard describes a workflow to create a Trusted BOM (TBOM)">>,
                  version      => ReSCALEVersion,
                  requirements => 
                      [#{'bom-ref'  => ReSCALEStandardConformanceURL,
                         identifier => <<"/rescale/", ReSCALEVersion/binary ,"/conformance/complete">>,
                         title      => <<"Full conformance with ReSCALE's 'complete' profile, e.g. complete absence of findings">>}]}]},
      declarations => 
          #{targets      => #{components => Targets},
            attestations => [
                #{
                    assessor => <<"Producer Reference">>,
                    summary  => <<"Mapping of Requirements to Claims">>,
                    map      => [
                      #{requirement   => ReSCALEStandardConformanceURL,
                        counterClaims => [Name || {Name, _Content } <- Claims]}]}],
            claims        => Claims,
            evidence      => Evidences}}.

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

% Retrieves the CycloneDX schema for the given specification version.
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

%--- Internal Fun.: Internal tools info

-spec generate_tool_info(Tool, Configuration) -> Result 
 when Tool           :: sscg_generator | static_code_analysis_module,
      Configuration  :: binary(),
      Result         :: map().
generate_tool_info(sscg_generator, Configuration) -> 
    {ok, Version} = sscg_generator_app_info:get_version(),
    VersionBinary = list_to_binary(Version),

    Name = atom_to_binary(sscg_generator_app_info:get_app_name(), utf8),

    #{type           => application,
      name           => <<"ReSCALE SSCG Generator">>,
      version        => VersionBinary,
      description    => <<"A ReSCALE certified tool to generate SSCGs">>,
      purl           => <<"pkg:hex/", Name/binary, "@", VersionBinary/binary>>,
      data           => [#{name => <<"CLI configuration flags">>,
                           type => configuration,
                           contents => 
                               #{attachment => #{content => Configuration}}}],
        % TODO: Generate it
      hashes         => [#{alg     => <<"SHA-1">>, 
                           content => <<"2fd4e1c67a2d28fced849ee1bb76e7391b93eb12">>}]
    };
% TODO: Dynamically generate this or extract to deps file
generate_tool_info(static_code_analysis_module = ToolName, Configuration) ->
    Version = <<"1.0.0">>,
    Name = atom_to_binary(ToolName, utf8),

    #{type        => container,
      name        => <<"ReSCALE Static Code Analysis Module">>,
      version     => Version, 
      description => <<"A ReSCALE certified container to execute static testing">>,
      purl        => <<"pkg:docker/", Name/binary, "@", Version/binary>>,
      data        => [#{name     => <<"Docker Environment">>, 
                        type     => configuration,
                        contents => 
                            #{attachment => #{content => Configuration}}}],
      % TODO: Generate it
      hashes     => [#{alg     => <<"SHA-1">>, 
                       content => <<"35d1c8f259129dc800ec8e073bb68f995424619c">>}]}.

%--- Internal Fun.: Tests Helpers

-spec process_test(TestData) -> Result
    when TestData :: test_format(),
         Result   :: #{claims   => [claim()],
                       evidence => [evidence()]}.
process_test(#{filename := FileName, 
               content  := Content, 
               format   := Format} = _TestData) -> 
        sscg_generator_test:process(Format, FileName, Content).

-spec extract_zip_file(binary()) -> 
    [test_format()].
extract_zip_file(FilePath) ->
    case zip_to_files(FilePath, [sarif, xml]) of  
        {ok, ExtractedFiles} -> ExtractedFiles;
        {error, Reason} ->
            sscg_generator_cli:abort("Failed to extract ZIP file ~s: ~p~n",
                                     [FilePath, Reason])
    end.

-spec prepare_csv_file(FilePath, FileName) -> Result
    when FilePath :: binary(),
         FileName :: binary(),
         Result   :: test_format().
prepare_csv_file(FilePath, FileName) ->
    case re:run(FileName, "(?i)codebert", [{capture, none}]) of
        match ->
            Content = case sscg_generator_csv:read(FilePath) of
                          {ok, Data}      -> Data;
                          {error, Reason} -> 
                               sscg_generator_cli:abort("Error: ~p", Reason)
                      end,
            #{filename    => FileName,
              content => Content, 
              format  => codebert};
        nomatch ->
            prepare_regular_file(FilePath, FileName)
    end.

-spec prepare_regular_file(FilePath, FileName) -> Result
when FilePath    :: binary(),
        FileName :: binary(),
        Result   :: test_format().
prepare_regular_file(FilePath, FileName) ->
    Content = read_file_content(FilePath),
    #{filename => FileName, content => Content, format => undefined}.

-spec zip_to_files(ZipFile, Extensions) -> Result
    when ZipFile    :: binary(),
         Extensions :: [binary()],
         Result     :: [test_format()] 
                       | {error, term()}.
zip_to_files(ZipBinary, Extensions) ->
    case zip:zip_open(ZipBinary, [memory]) of
        {ok, ZipHandle} ->
            case zip:zip_list_dir(ZipHandle) of
                {ok, FileList} ->
                    MatchingFiles = [File#zip_file.name 
                                     || File <- FileList, 
                                        matches_extension(File#zip_file.name, Extensions)],
                    lists:foreach(fun(FileName) ->
                        case zip:zip_get(FileName, ZipHandle) of
                            {ok, {FileName, FileData}} ->
                                #{filename => FileName, 
                                  content  => FileData, 
                                  format   => undefined}; %% TODO saster
                            {error, Reason} ->
                                {error, Reason}
                        end
                    end, MatchingFiles),
                    zip:zip_close(ZipHandle);
                {error, Reason} ->
                    {error, Reason}
                end;
        {error, Reason} ->  
            {error, Reason}
    end.

-spec matches_extension(FileName :: binary(), Extension :: [atom()]) -> boolean().
matches_extension(FileName, Extensions) ->
    FileExt = sscg_generator_utils:get_file_format(FileName),
    lists:member(FileExt, Extensions).

%--- Internal Fun.: Others

-spec read_file_content(binary()) -> binary().
read_file_content(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} -> Content;
        {error, Reason} ->
            sscg_generator_cli:abort("Failed to read file ~s: ~p~n", 
                                     [FilePath, Reason])
    end.
