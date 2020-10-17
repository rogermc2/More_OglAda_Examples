
// 3d title text shader

#version 410 core

layout(location = 0) in vec3 vp;
layout(location = 0) in vec3 vn;

uniform mat4 P, V, M;

out vec3 p_loc, n_loc;

void main ()
    {
	gl_Position = P * V * M * vec4 (vp, 1.0);
	p_loc = vp;
	n_loc = vn;
    }
