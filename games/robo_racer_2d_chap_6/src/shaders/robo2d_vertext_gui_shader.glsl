#version 410 core

layout (location = 0) in vec2 points2d;
layout (location = 1) in vec2 texcoord;

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

out vec2 v_texcoord;

void main()
    {
    gl_Position = projection_matrix * model_matrix * vec4(points2d, 0.0, 1.0);
    v_texcoord = texcoord;
    }
