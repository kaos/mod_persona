%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2013 Andreas Stenius
%% Date: 2013-02-06
%% @doc Enable Mozilla Persona logins.

%% Copyright 2013 Andreas Stenius
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_persona).
-author("Andreas Stenius <git@astekk.se>").

-mod_title("Mozilla Persona login support").
-mod_description("Allow users to login using their mozilla persona account.").
-mod_depends([base]).
-mod_provides([persona]).


%% interface functions
-export([
         event/2,
         observe_auth_logoff/3,
         observe_session_context/3
        ]).

-include_lib("zotonic.hrl").


event(#postback{ message={persona, [{event, Event}|Args]} }, Context) ->
    persona(Event, Args, Context).

observe_auth_logoff(auth_logoff, Context, _) ->
    case z_context:get_session(persona_login, Context) of
        true ->
            %% ?DEBUG("#### auth_logoff"),
            %% add script to let controller_logoff know we want to serve logoff.tpl
            z_script:add_script("// persona logout...", Context);
        _ -> Context
    end.

observe_session_context(session_context, Context, _) ->
    case z_context:get_session(persona_login, Context) of
        true ->
            case z_auth:user_from_session(Context) of
                undefined ->
                    z_script:add_script("navigator.id.logout();", Context);
                Id ->
                    z_context:set(persona_user, Id, Context)
            end;
        _ -> Context
    end.


%%====================================================================
%% support functions
%%====================================================================

persona("login", _Args, Context) ->
    Assertion = z_context:get_q(assertion, Context),
    {Msg, ContextMsg} = 
        case check_assertion(Assertion, Context) of
            {"okay", Info} -> 
                case do_persona_login(Info, Context) of
                    {ok, Ctx} -> 
                        z_context:set_session(persona_login, true, Ctx),
                        {"You have been logged in.",
                         z_render:wire({redirect, [{location, get_ready_page(Ctx)}]}, Ctx)};
                    {ok, M, Ctx} -> {M, Ctx};
                    {error, Err} -> {io_lib:format("Login failed: ~p.", [Err]), Context}
                end;
            {"failed", Info} -> 
                {io_lib:format("Login failed: ~p.", [proplists:get_value(reason, Info, "assertion failed")]), Context};
            Err -> 
                ?zWarning("Persona login assertion response error: ~p~n", [Err], Context),
                {"Login failed: bad verification response.", Context}
        end,
    ?DEBUG("#### persona login"),
    z_render:growl(Msg, ContextMsg);
persona("logout", _Args, Context) ->
    %% ?DEBUG("#### persona logout"),
    z_context:set_session(persona_login, undefined, Context),
    case z_auth:is_auth(Context) of
        true -> z_render:wire({redirect, [{dispatch, "logoff"}]}, Context);
        false -> Context
    end.

get_page(Context) ->
    case z_context:get_q("p", Context, []) of
        [] ->
            RD = z_context:get_reqdata(Context),
            case wrq:get_req_header("referer", RD) of
                undefined -> "/";
                Referrer -> Referrer
            end;
        Other ->
            Other
    end.

get_ready_page(Context) ->
    Page = get_page(Context),
    case z_notifier:first(#logon_ready_page{ request_page=Page }, Context) of
        undefined -> Page;
        Url -> Url
    end.
             
check_assertion(Assertion, Context) ->
    {Method, Req} = verify_req(Assertion, Context),
    Rsp = case httpc:request(Method, Req, [], []) of
              {ok, {{_, 200, _}, _Headers, Body}} ->
                  mochijson2:decode(Body);
              _Other ->
                  []
          end,
    case z_convert:convert_json(Rsp) of
        [{status, Status}|Info] ->
            {z_convert:to_list(Status), Info};
        Err -> Err
    end.

verify_req(Assertion, Context) ->
    Url = "https://verifier.login.persona.org/verify",
    Headers = [],
    ContentType = "application/x-www-form-urlencoded",
    Args = [{assertion, Assertion}, {audience, z_context:abs_url("", Context)}],
    {post, {Url, Headers, ContentType, mochiweb_util:urlencode(Args)}}.


%% Info is a proplist with these entries:
%% email		The address contained in the assertion, for the intended person being logged in.
%% audience		The audience value contained in the assertion. Expected to be your own website URL.
%% expires		The date the assertion expires, expressed as the primitive value of a Date object: that is, the number of milliseconds since midnight 01 January, 1970 UTC.
%% issuer		The hostname of the identity provider that issued the assertion.

do_persona_login(Info, Context) ->
    {email, Email} = proplists:lookup(email, Info),
    case {m_identity:lookup_by_type_and_key(email, Email, Context),
          z_context:get_session(persona_login, Context)} of
        {undefined, signup} -> 
            z_context:set_session(persona_login, undefined, Context),
            {ok, "Signup using your persona identity.", Context};
        {undefined, logout} -> 
            z_context:set_session(persona_login, undefined, Context),
            z_context:add_script_page("navigator.id.logout();", Context),
            {ok, "You are logged out.", Context};
        {undefined, _} ->
            case z_notifier:first(#signup_url{ 
                                     props=[{title, Email}, {email, Email}],
                                     signup_props=
                                         [
                                          {identity, 
                                           {username_pw, {z_utils:generate_username([{title, Email}], Context), z_ids:id(6)}, true, true}},
                                          {identity, {email, Email, true, true}}
                                          %%{ready_page, "..."}
                                         ]
                                    }, Context) of
                {ok, Location} ->
                    z_context:set_session(persona_login, signup, Context),
                    z_context:add_script_page(["window.location = \"", Location, "\";"], Context),
                    {ok, "Redirecting to signup page...", Context};
                undefined -> 
                    z_context:set_session(persona_login, logout, Context),
                    {ok, "Signups are disabled", Context}
            end;
        {IdProps, _} ->
            {rsc_id, RscId} = proplists:lookup(rsc_id, IdProps),
            z_auth:logon(RscId, Context)
    end.
