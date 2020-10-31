
#version 410 core

out vec4 frag_colour;

in vec2 st;

uniform sampler2D tex;

void main ()
    {
	frag_colour = texture (tex, st);
    frag_colour.rgb = frag_colour.rgb * 1.2;  //  orig 0.7
    }
