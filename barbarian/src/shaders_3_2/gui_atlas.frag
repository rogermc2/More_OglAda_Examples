//
// Crongdor the Barbarian
// texture-chooser shader
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D atlas;
uniform float alpha;

void main() {
	frag_colour = texture (atlas, st);
	frag_colour.a *= alpha;
}
