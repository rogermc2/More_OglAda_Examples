//
// Crongdor the Barbarian
// blood splatters that stick to walls
// First version Anton Gerdelan, 31 Jan 2014
// Latest code review 5 Feb 2015
//

#version 150

in vec3 vp;
in vec2 vt;
in vec3 vn;

uniform mat4 P, V;

out vec2 st;
out vec3 p_eye;
out vec3 n_eye;
out vec3 texcoords;

void main () {
	st = vt;
	p_eye = (V * vec4 (vp, 1.0)).xyz;
	n_eye = (V * vec4 (vn, 0.0)).xyz;
	gl_Position = P * vec4 (p_eye, 1.0);
	texcoords = vp;
}
