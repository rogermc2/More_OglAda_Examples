
#version 410 core

out vec4 frag_colour;

in vec2 st;

uniform vec4 text_colour;
uniform sampler2D tex;

void main ()
    {
	frag_colour = texture (tex, st) * text_colour;
    //frag_colour = texture (tex, st) * vec4 (1.0, 1.0, 1.0, 1.0);
   // frag_colour = text_colour;
    //    frag_colour = vec4(st[0], st[1], 0.0, 1.0);
    }
