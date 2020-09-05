//
// Crongdor the Barbarian
// camera frustum wireframe renderer
// First version Anton Gerdelan, 26 Dec 2014
// Latest code review
//

#version 150

in vec3 vp; // points

uniform mat4 PV; // model matrix

void main () {
	gl_Position = PV * vec4 (vp, 1.0);
}
