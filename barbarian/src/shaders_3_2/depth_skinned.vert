//
// Crongdor the Barbarian
// depth renderer for shadows
// First version Anton Gerdelan, ? 2013
// Latest code review 15 Dec 2014
//

#version 150

in vec3 vp;
in float bone_id;
uniform mat4 P, V, M, bone_matrices[32];
uniform vec3 light_pos_wor;
out vec3 dist_wor;

void main () {
	mat4 bone_mat = bone_matrices[int(bone_id)];
	vec3 pos_wor = vec3 (M * bone_mat * vec4 (vp, 1.0));
	dist_wor = pos_wor - light_pos_wor;
	gl_Position = P * V * vec4 (pos_wor, 1.0);
}
