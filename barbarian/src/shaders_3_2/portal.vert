
#version 410 core

layout(location = 0) in vec3 vp; // points
layout(location = 2) in vec2 vt; // tex coords
layout(location = 1) in vec3 vn; // normals

uniform mat4 M, V, P;

out vec2 st;
out vec3 n, p;

void main ()
    {
	st = vt;
	p = vp;
	gl_Position = P * V * M * vec4 (vp, 1.0);
    }
