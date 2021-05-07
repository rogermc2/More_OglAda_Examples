#version 410 core

out vec4 fragment_colour;
in vec3 v_colour;

void main()
    {
    fragment_colour = vec4(v_colour, 1.0);
    }
