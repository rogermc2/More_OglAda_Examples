
#version 410 core

layout(location = 0) in vec3 vp;

uniform mat4 PVM;

void main ()
    {
	gl_Position = PVM * vec4 (vp, 1.0);
    }
