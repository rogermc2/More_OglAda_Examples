#version 410 core

in vec4 f_color;

out vec4 FragColor;

uniform sampler2D mytexture;
uniform float sprite;

void main(void)
    {
	if (sprite > 1.0)
        FragColor = texture(mytexture, gl_PointCoord) * f_color;
	else
        FragColor = f_color;
    }
