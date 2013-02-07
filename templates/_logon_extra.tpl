<li id="logon_persona">
    <a id="{{ #persona_logon }}" href="#persona">
        <img src="/lib/images/persona_sign_in_blue.png" alt="Persona login" />
    </a>
</li>

{% wire id=#persona_logon
    action={mask target="logon_outer" message="Logon using persona..."}
    action={script script="navigator.id.request();"}
%}
