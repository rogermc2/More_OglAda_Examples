#version 410 core

uniform sampler2D tex0;
uniform sampler2D tex1;

in vec3 v_colour;
in vec2 v_texcoord;

out vec4 outColour;

void main()
    {
    outColour = mix(texture(tex0, Texcoord), texture(tex1, Texcoord), 0.5) * vec4(v_colour, 1.0);
    }
