
#version 410 core

layout(location = 0) in vec3 vp;
layout(location = 1) in vec3 vn;
layout(location = 2) in vec2 vt;
layout(location = 7) in vec3 smooth_vn;

uniform mat4 M, V, P;
uniform float ol_pass;

out vec3 p_eye; // TODO p_wor
out vec3 n_eye; // TODO n_wor
out vec2 st;
// shadow
// note this is just p_wor --> could move to world-space lighting calcs and
// lose a out
out vec3 texcoords; // TODO just use p_wor

// outlines silhoutte offset distance
float ol_dist = 0.03;

void main()
    {
	if (ol_pass > 0.1)
        {
		vec3 pos = vp + smooth_vn * ol_dist;
		gl_Position = P * V * M * vec4 (pos, 1.0);
		return;
        }

	p_eye = (V * M * vec4 (vp, 1.0)).xyz;
	n_eye = (V * M * vec4 (vn, 0.0)).xyz;
	st = vt;
	
	gl_Position = P * vec4 (p_eye, 1.0);
	texcoords = (M * vec4 (vp, 1.0)).xyz;
    }
