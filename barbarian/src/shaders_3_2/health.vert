//
// Crongdor the Barbarian
// health-bar shader
// First version Anton Gerdelan, ? 2012
// Latest code review 15 Dec 2014
//

#version 150

in vec2 vp;

uniform mat4 model_mat;

out vec2 st;

void main ()
    {
	st = vp * 0.5 + 0.5;
	gl_Position = model_mat * vec4 (vp, 0.0, 1.0);
    }
