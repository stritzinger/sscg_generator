-module(sscg_generator_test).

% API
-export([process/3]).

-include_lib("sscg_generator_types.hrl").

%--- API -----------------------------------------------------------------------
% @doc
% Processes test data in codebert o undefined format to transform it into 
% claims and evidence.
% @end
-spec process(Format, FileName, Data) -> Result
    when Format   :: undefined | codebert,
         FileName :: binary(),
         Data     :: binary(),
         Result   :: #{claims   := [claim()],
                       evidence := [evidence()]}.
process(Format, FileName, Data) ->
    generate_claims_and_evidence(Format, FileName, Data).

%--- Internal Functions --------------------------------------------------------

% Generates structured claims and evidence data from the parsed CSV data.
generate_claims_and_evidence(codebert = Format, Name, Data) ->
    TotalVulnerabilities = total_vulnerabilities(Data),

    if TotalVulnerabilities > 0 ->
        VulnerableRows = lists:filter(fun is_vulnerable/1, Data),
        EvidenceRefs = [create_evidence_ref(Format, Name, Row) 
                        || Row <- VulnerableRows],
        EvidenceList = [create_evidence(Format, Name, Row) 
                        || Row <- VulnerableRows],
        ClaimList = [#{'bom-ref'  => claim_ref(Name, #{num_vulnerabilities => TotalVulnerabilities}),
                        target    => undefined,
                        evidence  => EvidenceRefs}],
        #{claims => ClaimList, evidence => EvidenceList};
    true ->
        #{claims => [], evidence => []}
    end;
generate_claims_and_evidence(undefined = Format, Name, Data) ->
    EvidenceRefs = [create_evidence_ref(Format, Name)],
    EvidenceList = [create_evidence(Format, Name, #{content => Data})],
    Claims = [#{'bom-ref' => claim_ref(Name),
                target    => undefined,
                evidence  => EvidenceRefs}],
    #{claims   => Claims, 
      evidence => EvidenceList}.

% Calculates the total number of vulnerabilities in the given data.
-spec total_vulnerabilities([map()]) -> integer().
total_vulnerabilities(Data) ->
    lists:foldl(fun(Fields, Acc) ->
        case maps:get("Vulnerable", Fields, 0) of
            N when is_integer(N), N > 0 -> Acc + N;
            _ -> Acc
        end
    end, 0, Data).

is_vulnerable(Row) when is_map(Row) ->
    case maps:get("Vulnerable", Row, 0) of
        N when is_integer(N), N > 0 -> true;
        _ -> false
    end;
is_vulnerable(_) ->
    false.

create_evidence(codebert, 
                Name,
                #{"Function Name" := FunName, 
                  "Start Line"    := FunLine,
                  "Function Body" := Content} = _Row) ->

    #{'bom-ref'   => evidence_ref(#{filename      => Name,
                                    function_name => FunName,
                                    function_line => FunLine}),
      description => to_unicode_binary(
                         "Vulnerability found in ~s: function ~p starting at line ~p", 
                         [Name, FunName, FunLine]),
      data        => #{contents => 
                           #{attachment => 
                                 #{content => to_unicode_binary(Content)}}}};
create_evidence(undefined, 
                Name,
                #{content := Content} = _Row) ->

    #{'bom-ref'   => evidence_ref(#{filename => Name}),
      description => to_unicode_binary("Vulnerability found in ~s", [Name]),
      data        => #{contents => 
                           #{attachment => 
                                 #{content => to_unicode_binary(Content)}}}}.

create_evidence_ref(undefined, Name) -> create_evidence_ref(undefined, Name, #{}).
create_evidence_ref(undefined, Name, #{}) -> evidence_ref(#{filename => Name});
create_evidence_ref(codebert,
                    Name,
                    #{"Function Name" := FunName, "Start Line" := FunLine} = _Row) ->
    evidence_ref(#{filename      => Name, 
                   function_name => FunName, 
                   function_line => FunLine}).

evidence_ref(#{filename := Name} = Opts) ->
    case {maps:get(function_name, Opts, undefined), 
          maps:get(function_line, Opts, undefined)} of
        {undefined, undefined} ->
            to_unicode_binary("Evidence in ~s", [Name]);
        {FunName, undefined} ->
            to_unicode_binary("Evidence in ~s: Function ~p with no specified line.", 
                              [Name, FunName]);
        {undefined, Line} ->
            to_unicode_binary("Evidence in ~s: Line ~p with no specific function.",
                              [Name, Line]);
        {FunName, Line} ->
            to_unicode_binary("Evidence in ~s: Function ~p in line ~p.",
                              [Name, FunName, Line])
    end.

claim_ref(Name) -> claim_ref(Name, #{}).
claim_ref(Name, Options) ->
    VulnerabilityCount = maps:get(num_vulnerabilities, Options, -1),
    FormattedMessage = case VulnerabilityCount of
        -1 -> to_unicode_binary("Claim: ~s has found a vulnerability", [Name]);
        _  -> to_unicode_binary("Claim: ~s has found ~p vulnerabilities",
                               [Name, VulnerabilityCount])
    end,
    unicode:characters_to_binary(FormattedMessage).

to_unicode_binary(Content) when is_binary(Content); is_list(Content) -> 
    unicode:characters_to_binary(Content).

to_unicode_binary(Format, Args) when is_list(Format), is_list(Args) -> 
    unicode:characters_to_binary(io_lib:format(Format, Args)).