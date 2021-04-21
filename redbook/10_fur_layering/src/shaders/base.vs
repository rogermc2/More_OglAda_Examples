#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 tex_coord;

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

out vec3 normal_vs;

void main(void)
{
    normal_vs = normal;
    gl_Position = projection_matrix * (model_matrix * position);
}
