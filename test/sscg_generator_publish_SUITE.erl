-module(sscg_generator_publish_SUITE).

-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").

% Define
-define(MOCK_URL, <<"http://mock.endpoint">>).
-define(MOCK_JSONDATA, #{<<"key">> => <<"value">>}).
-define(MOCK_PATH, <<"/example/of/path/file.json">>).

% Test Setup
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

% Test Cases
-export([publish_success_test/1]).

%--- Test Setup ----------------------------------------------------------------
all() ->
    [publish_success_test].

init_per_testcase(_TestCase, Config) ->
    meck:new(hackney, [passthrough]),
    meck:new(sscg_generator_utils, [passthrough]),
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(sscg_generator_utils),
    meck:unload(hackney),
    Config.

%--- Test Cases ----------------------------------------------------------------
publish_success_test(_Config) ->

    meck:expect(sscg_generator_utils,
                read_json,
                fun(_Path) -> {ok, ?MOCK_JSONDATA} end),

    meck:expect(hackney,
                request,
                fun(post, _URL, _Headers, _Payload, _Opts) ->
                    {ok, 200, [], <<"{}">>}
                end),

    Result = sscg_generator_publish:publish(#{endpoint => ?MOCK_URL,
                                              sbom     => ?MOCK_PATH,
                                              sscg     => ?MOCK_PATH,
                                              token    => <<"mock_token">>}),

    ?assertEqual(ok, Result).