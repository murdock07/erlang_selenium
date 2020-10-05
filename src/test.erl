-module(test).

-export([random/0]).

random() ->
    webdriver:new_session(ts, firefox),
    webdriver:navigate_to(ts, <<"http://www.random.org/integers/">>),

    Min = webdriver:find_element(ts, <<"xpath">>, <<"/html/body/div/form/p[2]/input[1]">>),
    webdriver:element_clear(ts, Min),
    webdriver:element_send_keys(ts, Min, <<"5">>),

    Max = webdriver:find_element(ts, <<"xpath">>, <<"/html/body/div/form/p[2]/input[2]">>),
    webdriver:element_clear(ts, Max),
    webdriver:element_send_keys(ts, Max, <<"10">>),

    Button = webdriver:find_element(ts, <<"xpath">>, <<"/html/body/div/form/p[5]/input[1]">>),
    webdriver:element_click(ts, Button),

    Source = webdriver:get_page_source(ts),
    io:format(user, "Source: ~n~p~n", [Source]),

    webdriver:delete_session(ts).