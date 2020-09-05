//
// Crongdor the Barbarian
// shader for ambient sound sphere in editor
// First version Anton Gerdelan, 20 feb 2014
// Latest code review 14 Dec 2014
//

#version 410 core

in vec3 vp;

uniform mat4 PVM;

void main ()
    {
	gl_Position = PVM * vec4 (vp, 1.0);
    }
