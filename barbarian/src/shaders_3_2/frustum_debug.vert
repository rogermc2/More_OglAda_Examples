
// camera frustum wireframe renderer

#version 410 core

in vec3 vp; // points

uniform mat4 PV; // model matrix

void main ()
    {
	gl_Position = PV * vec4 (vp, 1.0);
    }
