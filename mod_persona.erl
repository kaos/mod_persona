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
         observe_postback_notify/2
        ]).

-include_lib("zotonic.hrl").

-define(PPRINT(V), ?PRINT(V)).
%-define(PPRINT(V), nop).

observe_postback_notify(#postback_notify{ message="persona.login" }, Context) ->
    Assertion = z_context:get_q(assertion, Context),
    ?PPRINT(Assertion),
    Msg = case check_assertion(Assertion, Context) of
              {"okay", Info} -> 
                  case persona_login(Info, Context) of
                      ok -> "You have been logged in.";
                      {ok, M} -> M;
                      Err -> io_lib:format("Login failed: ~p.", [Err])
                  end;
              {"failed", Info} -> 
                  io_lib:format("Login failed: ~p.", [proplists:get_value(reason, Info, "assertion failed")]);
              Err -> 
                  ?zWarning("Persona login assertion response error: ~p~n", [Err], Context),
                  "Login failed: bad verification response."
          end,
    z_render:growl(Msg, Context);
observe_postback_notify(#postback_notify{ message="persona.logout" }, Context) ->
    z_render:wire({redirect, [{dispatch, logoff}]}, Context);
observe_postback_notify(_, _Context) ->
    undefined.

%%====================================================================
%% support functions
%%====================================================================

check_assertion(Assertion, Context) ->
    {Method, Req} = verify_req(Assertion, Context),
    ?PPRINT(Req),
    Rsp = case httpc:request(Method, Req, [], []) of
              {ok, {{_, 200, _}, _Headers, Body}} ->
                  mochijson2:decode(Body);
              Other ->
                  ?PPRINT(Other),
                  []
          end,
    ?PPRINT(Rsp),
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

persona_login(Info, Context) ->
    ?PPRINT(Info),
    {email, Email} = proplists:lookup(email, Info),
    case {m_identity:lookup_by_type_and_key(email, Email, Context),
          z_context:get_session(persona_login, Context)} of
        {undefined, signup} -> 
            z_context:set_session(persona_login, undefined, Context),
            {ok, "Signup using your persona identity."};
        {undefined, logout} -> 
            z_context:set_session(persona_login, undefined, Context),
            z_context:add_script_page("navigator.id.logout();", Context),
            {ok, "You are logged out."};
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
                    ?PPRINT(Location),
                    z_context:set_session(persona_login, signup, Context),
                    z_context:add_script_page(["window.location = \"", Location, "\";"], Context),
                    {ok, "Redirecting to signup page..."};
                undefined -> 
                    z_context:set_session(persona_login, logout, Context),
                    "Signups are disabled"
            end;
        {Id, _} ->
            ?PPRINT(Id),
            case z_auth:logon(proplists:get_value(rsc_id, Id), Context) of
                {ok, _} -> ok;
                {error, Reason} -> Reason
            end
    end.

