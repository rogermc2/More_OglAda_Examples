#version 410 core

layout (location = 0) in vec3 points3d;
layout (location = 1) in vec2 texcoord;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

out vec2 v_texcoord;

void main()
    {
    gl_Position = projection_matrix * view_matrix * model_matrix * vec4(points3d, 1.0);
    v_texcoord = texcoord;
    }
