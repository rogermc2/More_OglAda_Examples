//
// Crongdor the Barbarian
// health-bar shader
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D tex_red, tex_base;
uniform float health_factor;

void main ()
    {
	vec4 a = texture (tex_red, st);
	vec4 b = texture (tex_base, st);
	float factor = 1.0;
	if (st.s < health_factor) { factor = 0.0; }
	frag_colour = mix (a, b, factor);
    }
