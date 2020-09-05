//
// Crongdor the Barbarian
// scrolling credits shader
// First version Anton Gerdelan, August 2014
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D tex;

void main () {
	frag_colour = texture (tex, st);
	frag_colour.rgb *= 0.7;
}
