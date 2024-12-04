%--- SSCG ----------------------------------------------------------------------
-doc "A map representing the Software Supply Chain Graph (SSCG) structure".
-type sscg() ::
    #{bomFormat    => binary(),
      specVersion  => version(),
      serialNumber => serial_number(),
      metadata     => #{timestamp  => timestamp(),
                        authors    => [#{name => binary(), email => binary()}],
                        tools      => #{components => [component()]}},
      definitions  => #{standards  => 
                            [#{'bom-ref'     => binary(),
                                name         => binary(),
                                description  => binary(),
                                version      => version(),
                                requirements => [#{'bom-ref'   => bom_ref(),
                                                    identifier => binary(),
                                                    title      => binary()}]
                              }]
                        },
      declarations => #{targets      => #{components => component()},
                        assessors    => [assessor()],
                        attestations => [#{assessor    => bom_ref(),
                                           summary     => binary(),
                                           map         => [#{requirement => binary(),
                                           claims      => [bom_ref()]}]
                                        }],
                        claims       => [claim()],
                        evidence     => [evidence()]}
    }.

-doc """
A map representing a component from the static code analysis module within the 
SSCG or a target component from the SBOM, representing a component of the 
tested software.
""".
-type component() :: #{bom_ref     => bom_ref(),
                       type        => binary(), % application, container, library or others.
                       name        => binary(),
                       version     => version(),
                       description => binary(),
                       purl        => binary(), % <<"pkg:docker/", Name/binary, "@", Version/binary>>
                       data        => [data()],
                       hashes      => [#{alg => binary(), content => binary()}]}.

-doc """
A map representing a claim generated from tests that detected an anomaly or
irregularity.
""".
-type claim() :: #{'bom-ref' => bom_ref(),
                   target    => undefined,
                   evidence  => [bom_ref()]}.

-doc """
Represents the evidence supporting a claim, containing the test content that 
substantiates it.
""".
-type evidence() :: #{'bom-ref'   => bom_ref(),
                      description => binary(),
                      data        => [data]
                    }.

-type assessor() :: #{'bom-ref'      => bom_ref(),
                      'third-party'  => binary(),
                      'organization' => #{'bom-ref' => bom_ref(), 
                                           name     => binary(), 
                                           contact  => binary()}}.

-type data()    :: #{name     => binary(),
                     type     => binary(), % configuration or others.
                     contents => #{attachment => #{content => binary()}}}.

-type bom_ref() :: binary().

%--- Files ---------------------------------------------------------------------
-type decoded_json() :: map().
-type file_path()    :: binary() | string().
-type folder_path()  :: binary() | string().

%--- Others --------------------------------------------------------------------
-type url()           :: binary().
-type version()       :: <<_ : _*8>>. % A version-like format, i.e., "1.0", "2.0"
-type timestamp()     :: binary().
-type serial_number() :: binary().