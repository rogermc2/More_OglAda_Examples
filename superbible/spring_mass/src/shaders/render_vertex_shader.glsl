#version 410 core

layout (location = 0) in vec3 position;
layout (location = 0) out vec3 v_position;

void main(void)
{
    gl_PointSize = 4.0;
    v_position = position;
    gl_Position = vec4(0.03 * position, 1.0);
}
