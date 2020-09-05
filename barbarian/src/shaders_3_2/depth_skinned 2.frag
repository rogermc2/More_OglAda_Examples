//
// Crongdor the Barbarian
// depth renderer for shadows
// First version Anton Gerdelan, ? 2013
// Latest code review 5 Feb 2016
//

#version 150
out vec4 frag_colour;

in vec3 dist_wor;

void main () {
	float l = length (dist_wor);
	frag_colour = vec4 (l, l, l, 1.0);
}
