-module(sscg_generator_http_SUITE).

-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").

% Define
-define(MOCK_URL, <<"http://mock.endpoint">>).
-define(MOCK_JSONDATA, #{}).

% Test Setup
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

% Test Cases
-export([successful_request_test/1,
         unexpected_status_code_test/1,
         request_failure_test/1,
         forbidden_request_test/1]).

%--- Test Setup ----------------------------------------------------------------
all() ->
    [successful_request_test,
     unexpected_status_code_test,
     request_failure_test].

init_per_testcase(_TestCase, Config) ->
    meck:new(hackney, [passthrough]),
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(hackney),
    Config.

%--- Test Cases ----------------------------------------------------------------
successful_request_test(_Config) ->
    meck:expect(hackney,
                request,
                fun(post, _URL, _Headers, _Payload, _Options) ->
                    {ok, 200, [], <<>>}
                end),
    ?assertEqual(ok, sscg_generator_http:post_json(?MOCK_URL, [], ?MOCK_JSONDATA)).

unexpected_status_code_test(_Config) ->
    meck:expect(hackney,
                request,
                fun(post, _URL, _Headers, _Payload, _Options) ->
                    {ok, 404, [], <<"Not Found">>}
                end),
    ?assertEqual({error, {unexpected_status, 404}},
                    sscg_generator_http:post_json(?MOCK_URL, [], ?MOCK_JSONDATA)).

request_failure_test(_Config) ->
    meck:expect(hackney,
                request,
                fun(post, _URL, _Headers, _Payload, _Options) ->
                    {error, timeout}
                end),
    ?assertEqual({error, {request_failed, timeout}},
                 sscg_generator_http:post_json(?MOCK_URL, [], ?MOCK_JSONDATA)).

forbidden_request_test(_Config) ->
    meck:expect(hackney,
                request,
                fun(post, _URL, _Headers, _Payload, _Options) ->
                    {ok, 403, [], <<"Forbidden">>}
                end),
    ?assertEqual({error, {unexpected_status, 403}},
                 sscg_generator_http:post_json(?MOCK_URL, [], ?MOCK_JSONDATA)).
