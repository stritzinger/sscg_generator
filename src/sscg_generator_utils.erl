% @doc Helper functions for the escript.
-module(sscg_generator_utils).

-export([current_timestamp/0]).
-export([prompt_for/1, prompt_for/3]).
-export([serial_number/0, uuid/0]).

current_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Timestamp =io_lib:format("~4..0B-~2..0B-~2..0B~ts~2..0B:~2..0B:~2..0BZ",
                             [Year, Month, Day, "T", Hour, Minute, Second]),
    iolist_to_binary(Timestamp).

prompt_for(VarName) ->
    sscg_generator_cli:print(color:blue(<<"Please enter the ", VarName/binary, ": ">>)),
    Var = sscg_generator_cli:input(""),
    binary:replace(Var, <<"\n">>, <<>>, [global]).

prompt_for(VarName, ValFun, Msg) ->
    sscg_generator_cli:print(color:blue(<<"Please enter the ", VarName/binary, ": ">>)),
    Var = sscg_generator_cli:input(""),
    CleanVar = binary:replace(Var, <<"\n">>, <<>>, [global]),
    
    case ValFun(CleanVar) of
        true ->
            CleanVar;
        false ->
            sscg_generator_cli:print("~s~n", [color:red(Msg)]),
            prompt_for(VarName, ValFun, Msg)  
    end.

uuid() -> uuid:uuid_to_string(uuid:get_v4(), binary_standard).

serial_number() ->
    UUID = uuid(),
    <<"urn:uuid-", UUID/binary>>.