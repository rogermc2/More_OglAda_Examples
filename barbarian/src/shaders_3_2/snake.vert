//
// Crongdor the Barbarian
// shader for snake
// First version Anton Gerdelan, 2 mar 2015
// Latest code review
//

#version 150

in vec3 vp;
in vec2 vt;
in vec3 vn;

uniform mat4 PVM;

out vec2 ft;
out vec3 fn;

void main () {
	gl_Position = PVM * vec4 (vp, 1.0);
	ft = vt;
	fn = vn;
}
