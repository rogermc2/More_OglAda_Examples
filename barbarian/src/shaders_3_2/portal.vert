//
// Crongdor the Barbarian
// wobbly portal shader
// First version Anton Gerdelan, 17 Jan 2015
// Latest code review
//
#version 410 core

in vec3 vp; // points
in vec2 vt; // tex coords
in vec3 vn; // normals

uniform mat4 M, V, P;

out vec2 st;
out vec3 n, p;

void main ()
    {
	st = vt;
	p = vp;
	gl_Position = P * V * M * vec4 (vp, 1.0);
    }
