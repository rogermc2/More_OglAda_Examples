//
// Crongdor the Barbarian
// 3d menu cursor
// First version Anton Gerdelan, ? 2014
// Latest code review 15 Dec 2014
//

#version 150

in vec3 vp, vn;
in vec2 vt;
in vec4 vtan;
uniform mat4 P, V, M;
out vec3 p_eye, n_eye;
out vec2 st;

void main () {
	p_eye = vec3 (V * M * vec4 (vp, 1.0));
	n_eye = (V * M * vec4 (vn, 0.0)).xyz;
	st = vt;
	gl_Position = P * vec4 (p_eye, 1.0);
}
