
#version 410 core

layout(location = 0) in vec2 vp;

out vec2 fp, st;

void main ()
    {
	fp = vp;
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp, 0.0, 1.0);
    }
