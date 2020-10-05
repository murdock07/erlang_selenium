-module(test).

-export([random/0]).

random() ->
    webdriver:new_session(ts, firefox),
    webdriver:navigate_to(ts, <<"http://www.random.org/integers/">>),
    webdriver:delete_session(ts).