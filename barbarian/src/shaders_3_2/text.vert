
#version 410 core

layout(location = 0) in vec2 vp;
layout(location = 2) in vec2 vt;

uniform vec2 pos;

out vec2 st;

void main ()
    {
 //   gl_PointSize = 20.0;
    st = vt;
	gl_Position = vec4 (pos + vp, 0.0, 1.0);
    }
