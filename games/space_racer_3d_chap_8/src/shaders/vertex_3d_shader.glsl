#version 410 core

layout (location = 0) in vec3 points3d;
layout (location = 1) in vec3 colours_in;

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

out vec3 colours;

void main()
    {
    gl_Position = projection_matrix * model_matrix * vec4(points3d, 1.0);
    colours = colours_in;
    }
