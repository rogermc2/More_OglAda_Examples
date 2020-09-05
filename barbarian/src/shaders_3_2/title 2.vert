//
// Crongdor the Barbarian
// 3d title text shader
// First version Anton Gerdelan, ? 2014
// Latest code review 28 Dec 2014
//

#version 150

in vec3 vp, vn;
uniform mat4 P, V, M;
out vec3 p_loc, n_loc;

void main () {
	gl_Position = P * V * M * vec4 (vp, 1.0);
	p_loc = vp;
	n_loc = vn;
}
