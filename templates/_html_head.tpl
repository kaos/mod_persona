{#
    The whole persona processing is a bit of a cludge;
    especially the logout handling, but also the automatic
    logon events. This because the logoff is triggered by
    visiting a special url, rather than on the click of a
    button, and persona is trigger happy!
#}

<script src="https://login.persona.org/include.orig.js"></script>
{% javascript %}
    var personaUser =
    {% if persona_user %}
        "{{ persona_user.email }}";
    {% else %}
        null;
    {% endif %}

    navigator.id.watch({
      loggedInUser: personaUser,
      onlogin: function(assertion) {
        z_event("persona.login", {assertion: assertion});
        {# default: logout persona if we're not authenticated on the server #}
        {% wire name="persona.login"
            action={script script="if (!personaUser) navigator.id.logout()"}
        %}
      },
      onlogout: function() {
        z_event("persona.logout");
        {% if persona_user %}
            {# default: notify backend #}
            {% wire name="persona.logout"
                delegate="mod_persona"
                postback={persona event="logout"}
            %}
        {% else %}
            {# default: do nothing #}
            {% wire name="persona.logout" action={script} %}
        {% endif %}
      },
      onready: function() {
        z_event("persona.ready");
        {# default: do nothing #}
        {% wire name="persona.ready" action={script} %}
      }
    });
{% endjavascript %}
