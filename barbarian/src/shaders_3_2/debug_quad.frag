//
// Crongdor the Barbarian
// debugging quad
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec3 st;
uniform samplerCube tex;

void main () {
	float texel = texture (tex, st).r;
	float c = texel * 0.01;
	frag_colour = vec4 (c, c, c, 1.0);
}
