
// 3d menu cursor

#version 410 core

in vec3 vp, vn;
in vec2 vt;
in vec4 vtan;

uniform mat4 P, V, M;

out vec3 p_eye, n_eye;
out vec2 st;

void main ()
    {
	p_eye = vec3 (V * M * vec4 (vp, 1.0));
	n_eye = (V * M * vec4 (vn, 0.0)).xyz;
	st = vt;
	gl_Position = P * vec4 (p_eye, 1.0);
    }