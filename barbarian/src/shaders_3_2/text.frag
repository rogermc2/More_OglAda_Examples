
#version 410 core

out vec4 frag_colour;

in vec2 st;

uniform vec4 text_colour;
uniform sampler2D tex;

void main ()
    {
	frag_colour = texture (tex, st) * text_colour;
    }
