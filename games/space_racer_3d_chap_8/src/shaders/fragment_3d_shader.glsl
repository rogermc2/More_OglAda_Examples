#version 410 core

out vec4 fragment_colour;

in vec3 colours;

void main()
    {
    fragment_colour = vec4(colours, 1.0);
    }
