//
// Crongdor the Barbarian
// 2d text shader background box
// First version Anton Gerdelan, ?10 Decemeber 2014
// Latest code review 15 Dec 2014
//

#version 410 core

in vec2 vp;

uniform vec2 scale, pos;
out vec2 fp;

void main ()
    {
	fp = vp;
	gl_Position = vec4 (vp * scale + pos, 0.0, 1.0);
    }
