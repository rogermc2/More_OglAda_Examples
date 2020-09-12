
#version 410 core
out vec4 frag_colour;

in vec3 dist_wor;

void main ()
    {
	float l = length (dist_wor);
	frag_colour = vec4 (l, l, l, 1.0);
    }
