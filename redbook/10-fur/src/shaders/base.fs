#version 410 core

layout (location = 0) out vec4 colour;

in vec3 normal_vs;

void main(void)
{
    vec3 normal = normal_vs;
    colour = vec4(0.2, 0.1, 0.5, 1.0) * (0.2 + pow(abs(normal.z), 4.0)) + vec4(0.8, 0.8, 0.8, 0.0) * pow(abs(normal.z), 137.0);
}
