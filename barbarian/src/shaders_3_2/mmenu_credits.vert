
#version 410 core

layout(location = 0) in vec2 vp;

out vec2 st;

uniform vec2 scale, pos;

void main ()
    {
   // gl_PointSize = 10.0;
    vec2 xy = vec2(vp[0] + 0.7, vp[1] + 0.2);
	st = 0.5 * (vp + 1.0);
	gl_Position = vec4 (vp * scale + pos, 0.0, 1.0);
    }
