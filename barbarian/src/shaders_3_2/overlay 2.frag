//
// Crongdor the Barbarian
// editor panel shader
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 texcoords;
uniform sampler2D atlas;
uniform float row, col;

void main() {

	frag_colour = texture (atlas, texcoords);
	if (frag_colour.a < 0.01) {
		discard;
	}
	if (texcoords.s >= col * 0.25 && texcoords.s < col * 0.25 + 0.25) {
		if (texcoords.t >= row * 0.25 && texcoords.t < row * 0.25 + 0.25) {
			// border highlight left
			if (texcoords.s >= col * 0.25 && texcoords.s < col * 0.25 + 0.01) {
				frag_colour.g += 0.75;
				frag_colour.a = 1.0;
				return;
			}
			// b.h. right
			if (texcoords.s < col * 0.25 + 0.25 && texcoords.s >= col * 0.25 + 0.24) {
				frag_colour.g += 0.75;
				frag_colour.a = 1.0;
				return;
			}
			// bh bottom
			if (texcoords.t >= row * 0.25 && texcoords.t < row * 0.25 + 0.01) {
				frag_colour.g += 0.75;
				frag_colour.a = 1.0;
				return;
			}
			// bh top
			if (texcoords.t < row * 0.25 + 0.25 && texcoords.t >= row * 0.25 + 0.24) {
				frag_colour.g += 0.75;
				frag_colour.a = 1.0;
				return;
			}
			return;
		}
	}
	frag_colour.rgb *= 0.5;
}
