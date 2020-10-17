
#version 410 core

layout(location = 0) in vec3 vp; // points
layout(location = 2) in vec2 vt; // tex coords
layout(location = 1) in vec3 vn; // normals

uniform mat4 M, V, P;
uniform float ol_pass;

float ol_dist = 0.01;

out vec2 st;
out vec3 n, p;

void main ()
    {
	if (ol_pass > 0.1)
        {
		vec3 p = vp + vn * ol_dist;
		gl_Position = P * V * M * vec4 (p, 1.0);
		return;
        }
	
	st = vt;
	n = vec3 (V * M * vec4 (vn, 0.0));
	p = vec3 (V * M * vec4 (vp, 1.0));
	gl_Position = P * V * M * vec4 (vp, 1.0);
    }
