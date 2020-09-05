//
// Crongdor the Barbarian
// animated sprite shader
// First version Anton Gerdelan, 31 Jan 2014
// Latest code review 5 Feb 2015
//

#version 150

in vec3 vp;
in vec3 vn;
in vec2 vt;

uniform mat4 P, V, M;
uniform vec2 sprite_st;
uniform float columns, rows, current_sprite;

out vec2 st;
out vec3 n_eye, p_eye; // TODO change to p_wor to save some outs
// shadow
out vec3 texcoords; // TODO p_wor use instead

void main() {
	//int sprite_col = mod (int (current_sprite), int (columns));
	int sprite_col = int (current_sprite - columns *
		floor (current_sprite / columns));
	int sprite_row = int (current_sprite) / int (columns);
	float fw = 1.0 / columns; // frame width
	float fh = 1.0 / rows;
	st = vec2 (fw * vt.s + fw * float (sprite_col),
		fh * vt.t + fh * float (rows - sprite_row - 1));
	
	n_eye = vec3 (V * M * vec4 (vn, 0.0));
	texcoords = vec3 (M * vec4 (vp, 1.0));
	p_eye = vec3 (V * vec4 (texcoords, 1.0));
	
	gl_Position = P * vec4 (p_eye, 1.0);
}
