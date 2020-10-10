
#version 410 core

in vec3 vp;
in vec3 vn;
in vec2 vt;
in float bone_id;

uniform mat4 P, V, M, bone_matrices[32];

uniform float ol_pass;
float ol_dist = 0.03;

out vec3 p_eye, n_eye;
out vec2 st;
// shadow
out vec3 texcoords; // TODO use p_wor

void main ()
    {
	mat4 bone_mat = bone_matrices[int(bone_id)];

	if (ol_pass > 0.1)
        {
		vec3 p = vp + vn * ol_dist;
		gl_Position = P * V * M * bone_mat * vec4 (p, 1.0);
		return;
        }

	p_eye = vec3 (V * M * bone_mat * vec4 (vp, 1.0));
	n_eye = vec3 (V * M * bone_mat * vec4 (vn, 0.0));
	st = vt;
	gl_Position = P * vec4 (p_eye, 1.0);
	
	texcoords = (M * bone_mat * vec4 (vp, 1.0)).xyz;
    }
