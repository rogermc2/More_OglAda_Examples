//
// Crongdor the Barbarian
// pass-through multi-pass shader
// First version Anton Gerdelan, ? 2013
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp;

out vec2 st;

void main () {
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp, 0.0, 1.0);
}
