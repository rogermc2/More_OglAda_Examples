//
// Crongdor the Barbarian
// 2d text shader
// First version Anton Gerdelan, Novemeber 2014
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D tex;
uniform vec4 text_colour;

void main () {
	frag_colour = texture (tex, st) * text_colour;
}
