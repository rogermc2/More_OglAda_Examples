#version 410 core

layout (location = 0) in vec3 v_position;

layout (location = 0) out vec4 colour;

void main(void)
{
    colour = vec4(v_position[0], 0.8, v_position[1], 1.0);
}
