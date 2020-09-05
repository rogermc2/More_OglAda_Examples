//
// Crongdor the Barbarian
// editor panel shader
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp;
in vec2 vt;

uniform vec2 scale;

out vec2 texcoords;

void main() {
	texcoords = vt;
	gl_Position = vec4 (vp * scale, 0.0, 1.0);
}
