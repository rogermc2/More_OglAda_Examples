#version 410 core

out vec4 fragment_colour;

in vec3 colour;

void main()
    {
    fragment_colour = vec4(colour, 1.0);
    }
