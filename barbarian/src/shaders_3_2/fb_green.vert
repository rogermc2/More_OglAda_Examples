
#version 410 core

in vec2 vp;
out vec2 st;

void main ()
    {
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp, 0.0, 1.0);
    }
