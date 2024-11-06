% Common types for SSCG project

%--- TEST ----------------------------------------------------------------------
- type test_format() ::  #{format   := undefined | codebert,
                           filename := binary(),
                           content  := binary()}.

%--- FILES ---------------------------------------------------------------------
-type decoded_json() :: map(). 
-type file_path()    :: binary(). 
-type folder_path()  :: binary(). 

%---- SSCG STRUCTURE -----------------------------------------------------------
-type bom_ref() :: binary().
-type spec_version() :: binary(). % A version-like format, i.e., "1.0", "2.0"
-type serial_number() :: binary().
-type timestamp() :: binary().

-type sscg_structure() :: #{
    bomFormat    => binary(),
    specVersion  => binary(),
    serialNumber => serial_number(),
    metadata     => #{timestamp => timestamp(),
                      authors   => [#{name  => binary(), 
                                      email => binary()}],
                      tools     => #{components => [component()]}},
    definitions  => #{
        standards => [#{'bom-ref'     => bom_ref(),
                         name         => binary(),
                         description  => binary(),
                         version      => binary(),
                         requirements => [#{'bom-ref'  => bom_ref(),
                                            identifier => identifier(),
                                            title      => binary()}]}]},
    declarations => #{
        targets        => #{components => [component()]},
        assesors       => #{'bom-ref'      => bom_ref(),
                             'third-party' => binary(),
                             organization  => #{'bom-ref' => bom_ref(),
                                                name      => binary(),
                                                contact   => binary()}},
        attestations   => [#{assessor => binary(),
                             summary  => binary(),
                             map      => [#{requirement   => binary(), 
                                            counterClaims => [binary()]}]}],
        claims        => [claim()],
        evidence      => [evidence()]
    }
}.

-type evidence() :: #{'bom-ref'   => bom_ref(),
                      description => binary(),
                      data        => #{contents => 
                                        #{attachment => 
                                            #{content => binary()}}}}.
-type claim() :: #{'bom-ref' := bom_ref(),
                   target    := binary() | undefined,
                   evidence  := [bom_ref()]}.

-type component() :: #{type        => binary(),
                       name        => atom(),
                       version     => binary(),
                       description => binary(),
                       purl        => binary(),
                       data        => data(),
                       hashes      => [#{alg     => binary(),
                                         content => binary()}]}.

-type data() :: #{name     => binary(),
                  type     => binary(),
                  contents := #{attachment => #{content => binary()}}}.
