-define(GECKO_DRIVER, "http://localhost:4444").
-define(FIREFOX_CAP, #{browserName => <<"firefox">>}).

-define(SESSION_ETS, sessions).
-define(SESSION_ETS_OPTS, [set, public, named_table]).

-record(capabilities, {
    browserName :: string(),
    browserVersion :: string(),
    platformName :: string(),
    acceptInsecureCerts :: boolean(),
    pageLoadStrategy :: string(),
    proxy :: proxy,
    setWindowRect :: boolean(),
    timeouts :: timeouts,
    strictFileInteractability :: boolean(),
    unhandledPromptBehavior :: string()
}).

-record(proxy, {
    proxyType = <<"autodetect">>, % pac|direct|autodetect|system|manual
    proxyAutoConfigUrl = <<"">>,
    ftpProxy = <<"">>,
    httpProxy = <<"">>,
    noProxy = [],
    sslProxy = <<"">>,
    socksProxy = <<"">>,
    socksVersion = 0
}).

-record(timeouts, {
    script = 30000,
    pageLoad = 300000,
    implicit = 0
}).