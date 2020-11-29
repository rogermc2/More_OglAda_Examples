#version 410 core

layout (location = 0) in vec2 coord2d;

out vec4 f_color;

uniform float offset_x;
uniform float scale_x;
uniform float sprite;

void main(void)
    {
	gl_Position = vec4((coord2d.x + offset_x) * scale_x, coord2d.y, 0, 1);
	f_color = 3 * vec4(coord2d.xy / 2.0 + 0.5, 0, 1);
	gl_PointSize = max(1.0, sprite);
    }
