//
// Crongdor the Barbarian
// 3d prop shader
// First version Anton Gerdelan, 31 Jan 2014
// Latest code review 9 Mar 2016
//

#version 150
#define NUM_LIGHTS 3

in vec3 vp;
in vec3 vn;
in vec2 vt;
in vec4 vtangent;

uniform mat4 P, V, M;
uniform float ol_pass;

out vec3 p_wor;
out vec3 fnormal;
out vec2 st;
out vec3 ftangent;

void main ()
    {
	if (ol_pass > 0.1)
        {
		float ol_dist = 0.03;
		vec3 p = vp + vn * ol_dist;
		gl_Position = P * V * M * vec4 (p, 1.0);
		return;
        }

	p_wor = vec3 (M * vec4 (vp, 1.0));
	fnormal = vn;
	st = vt;
	ftangent = vtangent.xyz;
	
	gl_Position = P * V * vec4 (p_wor, 1.0);
    }