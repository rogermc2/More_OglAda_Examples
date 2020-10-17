
#version 410 core

layout(location = 0) in vec2 vp;
layout(location = 2) in vec3 vt;

out vec3 st;

void main ()
    {
	st = vt;
	gl_Position = vec4 (vp, 0.0, 1.0);
    }
