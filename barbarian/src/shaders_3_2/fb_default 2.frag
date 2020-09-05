//
// Crongdor the Barbarian
// pass-through multi-pass shader
// First version Anton Gerdelan, ? 2013
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D tex;

void main () {
	frag_colour = texture (tex, st);
}
