% @doc Task to publish the SSCG .json and the SBOM in an URL.
-module(sscg_generator_publish).

% API
-export([cli/0, publish/1]).

-ifdef(TEST).
-export([group_by/3]).
-endif.

% Includes
-include_lib("kernel/include/logger.hrl").

% @doc Defines the CLI structure for the 'publish' command.
-spec cli() -> map().
cli() ->
    #{
        commands => #{
            "publish" => #{
                help => "Send the SSCG and the input SBOM to a URL",
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
                        name => sscg,
                        long => "-sscg",
                        short => $g,
                        help => {"[-g <SSCG_file>]", 
                                 fun() -> "SSCG file path" end},
                        type => binary,
                        required => true
                    },
                    #{
                        name => endpoint,
                        long => "-endpoint",
                        short => $e,
                        help => {"[-e <URL>]", 
                                 fun() -> "Endpoint URL where send the files " end},
                        type => binary,
                        required => true
                    }
                ]
            }
        }
    }.

publish(#{endpoint := Endpoint, sbom := SBOM_File, sscg := SSCG_File}) ->

    SBOMData = read_json_file(SBOM_File, "SBOM"),
    SSCGData = read_json_file(SSCG_File, "SSCG"),
    Data = #{sscg => SSCGData, sbom => SBOMData},

    case sscg_generator_http:post_json(Endpoint, Data) of
        ok -> 
            sscg_generator_cli:print("Request successfully sent. ~n");
        {error, {encode_error, Reason}} ->
            sscg_generator_cli:abort(
                "Error: Failed to encode JSON for the request. Data: ~p. Reason: ~p~n", 
                [Data, Reason]);
        {error, {request_failed, Reason}} ->
            sscg_generator_cli:abort(
                "Error: HTTP request to endpoint '~s' failed. Reason: ~p~n", 
                [Endpoint, Reason]);
        {error, {unexpected_status, StatusCode}} ->
            sscg_generator_cli:abort(
                "Error: Unexpected HTTP status code ~p received from endpoint '~s'.~n", 
                [StatusCode, Endpoint]);
        {error, {body_read_error, Reason}} ->
            sscg_generator_cli:abort(
                "Error: Failed to read the response body from endpoint '~s'. Reason: ~p~n", 
                [Endpoint, Reason]);
        {error, {invalid_json, Reason}} ->
            sscg_generator_cli:abort(
                "Error: The response from endpoint '~s' contains invalid JSON. Reason: ~p~n", 
                [Endpoint, Reason])
    end.

% Helper function to read and validate JSON files
-spec read_json_file(binary(), binary()) -> map() | no_return().
read_json_file(File, Type) ->
    case sscg_generator_utils:read_json(File) of
        {ok, Data} -> 
            Data;
        {error, {file_not_available, Reason}} ->
            sscg_generator_cli:abort(
                "Error: Unable to read ~s file '~s'. Reason: ~p~n", 
                [Type, File, Reason]);
        {error, invalid_json} ->
            sscg_generator_cli:abort(
                "Error: ~s file '~s' contains invalid JSON format.~n", 
                [Type, File])
    end.