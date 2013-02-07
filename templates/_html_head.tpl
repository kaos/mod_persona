<script src="https://login.persona.org/include.js"></script>

{% javascript %}
var personaUser = "{% if m.acl.user %}{{ m.acl.user.email }}{% endif %}";

navigator.id.watch({
    loggedInUser: personaUser,
    onlogin: function(assertion) {
        z_notify("persona.login", {assertion: assertion});
    },
    onlogout: function() {
        z_notify("persona.logout");
    }
});
{% endjavascript %}
