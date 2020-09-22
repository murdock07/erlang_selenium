-module(random_org_test).

-export([test_c/0,
         test_f/0,
         test_f_custom/0]).

-define(CHROMEDRIVER, "http://localhost:9515/").
-define(FIREFOXDRIVER, "http://localhost:4444/").

-include_lib("webdrv/include/webdrv.hrl").

test_f() ->
    {ok, _Pid} = webdrv_session:start_session(test_f, ?FIREFOXDRIVER,  webdrv_cap:default_firefox(), 10000),
    webdrv_session:set_url(test_f, "http://www.random.org/integers/"),
    {ok, Emin} = webdrv_session:find_element(test_f, "name", "min"),
    webdrv_session:clear_element(test_f, Emin),
    webdrv_session:send_value(test_f, Emin, "5"),
    {ok, Emax} = webdrv_session:find_element(test_f, "name", "max"),
    webdrv_session:clear_element(test_f, Emax),
    webdrv_session:send_value(test_f, Emax, "15"),
    webdrv_session:submit(test_f, Emax),
    {ok, PageSource} = webdrv_session:get_page_source(test_f),
    string:str(PageSource, "Here are your random numbers"),
    webdrv_session:stop_session(test_f).

test_f_custom() ->
    Capability = #{firstMatch => [#{browserName => <<"firefox">>}]},
    {ok, _Pid} = webdrv_session:start_session(test_f_custom, ?FIREFOXDRIVER,  Capability, 10000),
    webdrv_session:set_url(test_f_custom, "http://www.random.org/integers/"),
    {ok, Emin} = webdrv_session:find_element(test_f_custom, "name", "min"),
    webdrv_session:clear_element(test_f_custom, Emin),
    webdrv_session:send_value(test_f_custom, Emin, "5"),
    {ok, Emax} = webdrv_session:find_element(test_f_custom, "name", "max"),
    webdrv_session:clear_element(test_f_custom, Emax),
    webdrv_session:send_value(test_f_custom, Emax, "15"),
    webdrv_session:submit(test_f_custom, Emax),
    {ok, PageSource} = webdrv_session:get_page_source(test_f_custom),
    string:str(PageSource, "Here are your random numbers"),
    webdrv_session:stop_session(test_f_custom).

test_c() ->
    {ok, _Pid} = webdrv_session:start_session(test_c, ?CHROMEDRIVER,  webdrv_cap:default_chrome(), 10000),
    webdrv_session:set_url(test_c, "http://www.random.org/integers/"),
    {ok, Emin} = webdrv_session:find_element(test_c, "name", "min"),
    webdrv_session:clear_element(test_c, Emin),
    webdrv_session:send_value(test_c, Emin, "5"),
    {ok, Emax} = webdrv_session:find_element(test_c, "name", "max"),
    webdrv_session:clear_element(test_c, Emax),
    webdrv_session:send_value(test_c, Emax, "15"),
    webdrv_session:submit(test_c, Emax),
    {ok, PageSource} = webdrv_session:get_page_source(test_c),
    string:str(PageSource, "Here are your random numbers"),
    webdrv_session:stop_session(test_c).