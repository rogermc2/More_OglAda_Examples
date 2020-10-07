
#version 410 core

out vec4 frag_colour;

in vec3 st;

uniform samplerCube tex;

void main ()
    {
	float texel = texture (tex, st).r;
	float c = texel * 0.01;
	frag_colour = vec4 (c, c, c, 1.0);
    }
