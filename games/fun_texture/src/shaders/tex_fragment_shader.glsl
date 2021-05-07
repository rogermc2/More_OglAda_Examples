#version 410

in vec3 colours;
in vec2 texcoord;

uniform sampler2D texture;

out vec4 fragment_colour;

void main()
    {
   fragment_colour = vec4(colours, 1.0);
    }
