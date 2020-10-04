
#version 410 core

in vec2 vp;

uniform mat4 model_mat;
uniform float columns, current_sprite;

out vec2 st;

void main()
    {
	//int sprite_col = int (current_sprite) % int (columns);
	int sprite_col = int (current_sprite - columns * floor (current_sprite / columns));
	int sprite_row = int (current_sprite) / int (columns);
	float fw = 1.0 / columns; // frame width
	vec2 vt = vp * 0.5 + 0.5;
	st = vec2 (fw * vt.s + fw * float (sprite_col), fw * vt.t + fw * float (columns - sprite_row - 1));
	
	gl_Position = model_mat * vec4 (vp, 0.0, 1.0);
    }
