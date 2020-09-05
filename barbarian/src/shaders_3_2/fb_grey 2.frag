//
// Crongdor the Barbarian
// greyscale multi-pass shader
// First version Anton Gerdelan, 12 Mar 2015
// Latest code review
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform sampler2D tex;

void main () {
	vec4 rgba = texture (tex, st);
	float y = 0.2126 * rgba.r + 0.7152 * rgba.g + 0.0722 * rgba.b;
	frag_colour = vec4 (y, y, y, rgba.a);
}
