{# override persona ready event handler #}
{% wire name="persona.ready"
    action={script script="navigator.id.logout()"}
%}
