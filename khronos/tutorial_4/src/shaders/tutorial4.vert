#version 410 core
layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Colour;

flat out vec3 geom_Colour;

uniform mat4 mvp_matrix;

void main(void)
    {
    gl_Position = vec4(in_Position, 1.0);

    geom_Colour = in_Colour;
    }
