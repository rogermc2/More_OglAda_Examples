#version 410 core

in vec2 v_texcoord;

uniform vec4 ambient;
uniform sampler2D texture2d;

out vec4 fragment_colour;

void main()
    {
    fragment_colour = ambient + texture(texture2d, v_texcoord);
    }
