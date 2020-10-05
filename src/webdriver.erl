-module(webdriver).

-export([
    new_session/2,
    new_session/3,
    delete_session/1,
    status/0,
    get_timeouts/0,
    set_timeouts/0,
    navigate_to/2,
    get_current_url/0,
    back/0,
    forward/0,
    refresh/0,
    get_title/0,
    get_window_handle/0,
    close_window/0,
    switch_to_window/0,
    get_window_handles/0,
    new_window/0,
    switch_to_frame/0,
    switch_to_parent_frame/0,
    get_window_rect/0,
    set_window_rect/0,
    maximize_window/0,
    minimize_window/0,
    fullscreen_window/0,
    get_active_element/0,
    find_element/3,
    find_elements/0,
    find_element_from_element/0,
    find_elements_from_element/0,
    is_element_selected/0,
    get_element_attribute/0,
    get_element_property/0,
    get_element_css_value/0,
    get_element_text/2,
    get_element_tag_name/0,
    get_element_rect/0,
    is_element_enabled/0,
    get_computed_role/0,
    get_computed_label/0,
    element_click/2,
    element_clear/2,
    element_send_keys/3,
    get_page_source/1,
    execute_script/0,
    execute_script_async/0,
    get_all_cookies/0,
    get_named_cookies/0,
    add_cookie/0,
    delete_cookie/0,
    delete_all_cookies/0,
    perform_actions/0,
    release_actions/0,
    dismiss_alert/0,
    accept_alert/0,
    get_alert_text/0,
    send_alert_text/0,
    take_screenshot/0,
    teke_element_screenshot/0,
    print_page/0
]).

-include("webdriver.hrl").

new_session(SessionName, firefox) ->
    new_session(SessionName, ?GECKO_DRIVER, ?FIREFOX_CAP).

new_session(SessionName, Driver, Capabilities) -> 
    Url = Driver ++ "/session",
    Result = wrapper:post_request(Url, Capabilities),
    SessionId = maps:get(<<"sessionId">>, Result, null),
    ReCapabilities = maps:get(<<"capabilities">>, Result, null),
    wrapper:store_session({SessionName, Driver, SessionId, ReCapabilities}).

delete_session(SessionName) ->
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId),
    wrapper:delete_request(Url),
    wrapper:clear_session(SessionName).

status() -> 
    % GET 	/status
    ok.
get_timeouts() -> 
    % GET 	/session/{session id}/timeouts
    ok.
set_timeouts() -> 
    % POST 	/session/{session id}/timeouts
    ok.
navigate_to(SessionName, TargetUrl) ->
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++ "/url",
    wrapper:post_request(Url, #{<<"url">> => TargetUrl}).

get_current_url() -> 
    % GET 	/session/{session id}/url
    ok.
back() -> 
    % POST 	/session/{session id}/back
    ok.
forward() -> 
    % POST 	/session/{session id}/forward
    ok.
refresh() -> 
    % POST 	/session/{session id}/refresh
    ok.
get_title() -> 
    % GET 	/session/{session id}/title
    ok.
get_window_handle() -> 
    % GET 	/session/{session id}/window
    ok.
close_window() -> 
    % DELETE 	/session/{session id}/window
    ok.
switch_to_window() -> 
    % POST 	/session/{session id}/window
    ok.
get_window_handles() -> 
    % GET 	/session/{session id}/window/handles
    ok.
new_window() -> 
    % POST 	/session/{session id}/window/new
    ok.
switch_to_frame() -> 
    % POST 	/session/{session id}/frame
    ok.
switch_to_parent_frame() -> 
    % POST 	/session/{session id}/frame/parent
    ok.
get_window_rect() -> 
    % GET 	/session/{session id}/window/rect
    ok.
set_window_rect() -> 
    % POST 	/session/{session id}/window/rect
    ok.
maximize_window() -> 
    % POST 	/session/{session id}/window/maximize
    ok.
minimize_window() -> 
    % POST 	/session/{session id}/window/minimize
    ok.
fullscreen_window() -> 
    % POST 	/session/{session id}/window/fullscreenPOST 	/session/{session id}/window/fullscreen
    ok.
get_active_element() -> 
    % GET 	/session/{session id}/element/active
    ok.
find_element(SessionName, LocationStrategy, Selector) -> 
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++ "/element",
    ElemMap = wrapper:post_request(Url, #{<<"using">> => LocationStrategy, <<"value">> => Selector}),
    [{_Key, Value}] = maps:to_list(ElemMap),
    Value.

find_elements() -> 
    % POST 	/session/{session id}/elements
    ok.
find_element_from_element() -> 
    % POST 	/session/{session id}/element/{element id}/element
    ok.
find_elements_from_element() -> 
    % POST 	/session/{session id}/element/{element id}/elements
    ok.
is_element_selected() -> 
    % GET 	/session/{session id}/element/{element id}/selected
    ok.
get_element_attribute() -> 
    % GET 	/session/{session id}/element/{element id}/attribute/{name}
    ok.
get_element_property() -> 
    % GET 	/session/{session id}/element/{element id}/property/{name}
    ok.
get_element_css_value() -> 
    % GET 	/session/{session id}/element/{element id}/css/{property name}
    ok.

get_element_text(SessionName, ElementId) -> 
    % GET 	/session/{session id}/element/{element id}/text
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++
                    "/element/" ++ binary_to_list(ElementId) ++ "/text",
    wrapper:get_request(Url).

get_element_tag_name() -> 
    % GET 	/session/{session id}/element/{element id}/name
    ok.
get_element_rect() -> 
    % GET 	/session/{session id}/element/{element id}/rect
    ok.
is_element_enabled() -> 
    % GET 	/session/{session id}/element/{element id}/enabled
    ok.
get_computed_role() -> 
    % GET 	/session/{session id}/element/{element id}/computedrole
    ok.
get_computed_label() -> 
    % GET 	/session/{session id}/element/{element id}/computedlabel
    ok.

element_click(SessionName, ElementId) -> 
    % POST 	/session/{session id}/element/{element id}/click
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++
                    "/element/" ++ binary_to_list(ElementId) ++ "/click",
    wrapper:post_request(Url, #{}).

element_clear(SessionName, ElementId) ->
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++
                    "/element/" ++ binary_to_list(ElementId) ++ "/clear",
    wrapper:post_request(Url, #{}).

element_send_keys(SessionName, ElementId, Value) ->
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++
                    "/element/" ++ binary_to_list(ElementId) ++ "/value",
    wrapper:post_request(Url, #{<<"text">> => Value}).

get_page_source(SessionName) ->
    {Driver, SessionId} = wrapper:get_session(SessionName),
    Url = Driver ++ "/session/" ++ binary_to_list(SessionId) ++ "/source",
    wrapper:get_request(Url).

execute_script() -> 
    % POST 	/session/{session id}/execute/sync
    ok.
execute_script_async() -> 
    % POST 	/session/{session id}/execute/async
    ok.
get_all_cookies() -> 
    % GET 	/session/{session id}/cookie
    ok.
get_named_cookies() -> 
    % GET 	/session/{session id}/cookie/{name}
    ok.
add_cookie() -> 
    % POST 	/session/{session id}/cookie
    ok.
delete_cookie() -> 
    % DELETE 	/session/{session id}/cookie/{name}
    ok.
delete_all_cookies() -> 
    % DELETE 	/session/{session id}/cookie
    ok.
perform_actions() -> 
    % POST 	/session/{session id}/actions
    ok.
release_actions() -> 
    % DELETE 	/session/{session id}/actions
    ok.
dismiss_alert() -> 
    % POST 	/session/{session id}/alert/dismiss
    ok.
accept_alert() -> 
    % POST 	/session/{session id}/alert/accept
    ok.
get_alert_text() -> 
    % GET 	/session/{session id}/alert/text
    ok.
send_alert_text() -> 
    % POST 	/session/{session id}/alert/text
    ok.
take_screenshot() -> 
    % GET 	/session/{session id}/screenshot
    ok.
teke_element_screenshot() -> 
    % GET 	/session/{session id}/element/{element id}/screenshot
    ok.
print_page() -> 
    % POST 	/session/{session id}/print
    ok.