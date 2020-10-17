
#version 410 core

layout(location = 0) in vec2 vp;
layout(location = 2) in vec2 vt;

uniform vec2 scale;

out vec2 texcoords;

void main()
    {
	texcoords = vt;
	gl_Position = vec4 (vp * scale, 0.0, 1.0);
    }
