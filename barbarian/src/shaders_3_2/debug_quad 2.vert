//
// Crongdor the Barbarian
// debugging quad
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp;
in vec3 vt;
out vec3 st;

void main () {
	st = vt;
	gl_Position = vec4 (vp, 0.0, 1.0);
}
