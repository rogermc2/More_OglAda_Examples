//

#version 410 core

layout(location = 0) in vec3 vp;
layout(location = 2) in vec2 vt;
layout(location = 1) in vec3 vn;

uniform mat4 PVM;

out vec2 ft;
out vec3 fn;

void main () {
	gl_Position = PVM * vec4 (vp, 1.0);
	ft = vt;
	fn = vn;
}
