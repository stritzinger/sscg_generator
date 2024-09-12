-module(sscg_generator_http).

% API
-export([get_json/1]).

% Include any necessary libraries
-include_lib("kernel/include/logger.hrl").

% Perform an HTTP GET request and return the JSON response body
get_json(URL) ->
    Method = get,
    Headers = [],  % No custom headers needed
    Payload = <<>>,  % No payload for GET requests
    Options = [{follow_redirect, true}],  % Follow redirects if necessary

    % Make the request
    case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->  % If status is 200 (OK)
            case hackney:body(ClientRef) of
                {ok, Body} -> 
                    % Return the raw body
                    {ok, Body};
                {error, Reason} -> 
                    {error, {body_read_error, Reason}}
            end;
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            % If a non-200 status is returned
            {error, {unexpected_status, StatusCode}};
        {error, Reason} ->
            % If the request failed
            {error, {request_failed, Reason}}
    end.