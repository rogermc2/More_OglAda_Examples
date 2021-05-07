#version 410

in vec3 colours;

out vec4 fragment_colour;

void main()
    {
   fragment_colour = vec4(colours, 1.0);
    }
