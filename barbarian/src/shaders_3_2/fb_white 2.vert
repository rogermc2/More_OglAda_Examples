//
// Crongdor the Barbarian
// flash white multi-pass shader
// First version Anton Gerdelan, 10 apr 2015
// Latest code review
//

#version 150

in vec2 vp;
out vec2 st;

void main () {
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp, 0.0, 1.0);
}
