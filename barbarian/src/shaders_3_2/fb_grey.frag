
#version 410 core

out vec4 frag_colour;

in vec2 st;

uniform sampler2D tex;

void main ()
    {
	vec4 rgba = texture (tex, st);
	float y = 0.2126 * rgba.r + 0.7152 * rgba.g + 0.0722 * rgba.b;
	frag_colour = vec4 (y, y, y, rgba.a);
    }
