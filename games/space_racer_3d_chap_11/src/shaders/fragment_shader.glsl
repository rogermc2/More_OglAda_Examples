#version 410 core

out vec4 fragment_colour;

in vec3 frag_colour;

void main()
    {
    fragment_colour = vec4(frag_colour, 1.0);
    }
