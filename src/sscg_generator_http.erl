-module(sscg_generator_http).

-export([get_json/1]).

-include_lib("kernel/include/logger.hrl").

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