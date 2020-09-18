

#version 410 core

in vec3 vp;

uniform mat4 P, V, M;
uniform vec3 light_pos_wor;
out vec3 dist_wor;

void main ()
    {
	vec3 pos_wor = vec3 (M * vec4 (vp, 1.0));
	dist_wor = pos_wor - light_pos_wor;
	gl_Position = P * V * vec4 (pos_wor, 1.0);
    }