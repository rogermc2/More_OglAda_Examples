#version 410 core
layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Colour;

uniform mat4 mvp_matrix;

out vec3 ex_Colour;

void main(void)
    {
    // As flat lines are used the input only has two points: x and y.
    // Set the Z coordinate to 0 and the W coordinate to 1
    gl_Position = mvp_matrix * vec4(in_Position, 1.0);

    ex_Colour = in_Colour;
    }
