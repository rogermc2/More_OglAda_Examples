
#version 410 core

layout(location = 0) in vec2 vp;

uniform vec2 scale, pos;
out vec2 fp;

void main ()
    {
	fp = vp;
	gl_Position = vec4 (vp * scale + pos, 0.0, 1.0);
    }
