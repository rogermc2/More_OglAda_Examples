
#version 410 core
out vec4 frag_colour;

in vec2 st;
uniform sampler2D atlas;
uniform float alpha;

void main()
    {
	frag_colour = texture (atlas, st);
	frag_colour.a *= alpha;
    }
