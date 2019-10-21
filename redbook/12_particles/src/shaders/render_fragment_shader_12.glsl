#version 410 core

layout (location = 0) out vec4 colour;

in float intensity;

void main(void)
    {
    colour = mix (vec4(0.0, 0.2, 1.0, 1.0), vec4(0.2, 0.05, 0.0, 1.0), intensity);
    }
