#version 410 core

layout (location = 0) in vec4 vertex;

uniform mat4 mvp;

out float intensity;

void main(void)
    {
    intensity = vertex.w;
    gl_Position = mvp * vec4 (vertex.xyz, 1.0);
    }
