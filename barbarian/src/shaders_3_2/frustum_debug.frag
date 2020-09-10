// camera frustum wireframe renderer

#version 410 core
out vec4 frag_colour;

uniform vec4 colour;

void main ()
    {
	frag_colour = colour;
    }
