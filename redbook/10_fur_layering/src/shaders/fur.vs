#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 tex_coord;

out VS_GS_INTERFACE
{
    vec3 normal;
    vec2 tex_coord;
} vertex_vs;

void main(void)
{
    vertex_vs.normal = normal;
    vertex_vs.tex_coord = tex_coord;
    gl_Position = position;
}
