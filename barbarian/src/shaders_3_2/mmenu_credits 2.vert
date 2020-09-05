//
// Crongdor the Barbarian
// scrolling credits shader
// First version Anton Gerdelan, August 2014
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp;
out vec2 st;
uniform vec2 scale, pos;

void main () {
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp * scale + pos, 0.0, 1.0);
}
