-module(sscg_generator_http).

-export([get_json/1, post_json/2]).

% @doc
% Perform an HTTP GET request to a JSON resource and return the decoded JSON.
%
% This function performs a request to the provided URL and decodes the response
% body if it is in valid JSON format. It handles errors such as request failures,
% invalid response status codes, and JSON decoding errors.
-spec get_json(URL) -> Result
    when URL    :: binary(),
         Result :: {ok, map()}
                   | {error, {request_failed,    Reason}}
                   | {error, {unexpected_status, StatusCode}}
                   | {error, {body_read_error,   Reason}}
                   | {error, {invalid_json,      Reason}},
        Reason :: term(),
        StatusCode :: non_neg_integer().
get_json(URL) ->
    Method = get,
    Headers = [],
    Payload = <<>>,  % No payload for GET requests
    Options = [{follow_redirect, true}],  % Follow redirects if necessary

    case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    try
                        {ok, jsone:decode(Body)}
                    catch
                        _:{error, Reason} ->
                            {error, {invalid_json, Reason}}
                    end;
                {error, Reason} ->
                    {error, {body_read_error, Reason}}
            end;
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            {error, {unexpected_status, StatusCode}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

% @doc
% Perform an HTTP POST request with a JSON payload and return the response.
%
% This function sends a JSON-encoded payload to the provided URL. It handles
% errors such as request failures and invalid response status codes.
-spec post_json(URL, JsonData) -> Result
    when URL        :: binary(),
         JsonData   :: map(),
         Result     :: ok
                       | {error, {encode_error,      Reason}}
                       | {error, {request_failed,    Reason}}
                       | {error, {unexpected_status, StatusCode}},
         Reason     :: term(),
         StatusCode :: non_neg_integer().
post_json(URL, JsonData) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    EncodeOpts = [{indent, 4},
                  {float_format, [{scientific, 2}]},
                  native_forward_slash,
                  skip_undefined],
    try
        JsonPayload = jsone:encode(JsonData, EncodeOpts),
        Options = [{follow_redirect, true}],
        case hackney:request(post, URL, Headers, JsonPayload, Options) of
            {ok, 200, _RespHeaders, _ClientRef} ->
                ok;
            {ok, StatusCode, _RespHeaders, _ClientRef} ->
                {error, {unexpected_status, StatusCode}};
            {error, Reason} ->
                {error, {request_failed, Reason}}
        end
    catch
        _:{error, EncodingReason} ->
            {error, {encode_error, EncodingReason}}
    end.
