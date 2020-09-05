//
// Crongdor the Barbarian
// 2d text shader
// First version Anton Gerdelan, Novemeber 2014
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp, vt;
uniform vec2 pos;
out vec2 st;

void main ()
    {
	st = vt;
	gl_Position = vec4 (pos + vp, 0.0, 1.0);
    }
