
#version 410 core

layout(location = 0) in vec3 vp;

uniform mat4 M;
uniform mat4 V;
uniform mat4 P;

void main ()
    {
	gl_Position = P * V * M * vec4 (vp, 1.0);
    }
