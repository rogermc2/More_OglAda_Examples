#version 410 core
// in_Position was bound to attribute index 0
// in_Color was bound to attribute index 1
layout(location = 0) in vec2 in_Position;
layout(location = 1) in vec3 in_Colour;

// Set the ex_Colour variable to the next shader in the chain
out vec3 ex_Colour;

void main(void)
    {
    // As flat lines are used the input only has two points: x and y.
    // Set the Z coordinate to 0 and the W coordinate to 1

    gl_Position = vec4(in_Position.x, in_Position.y, 0.0, 1.0);

    // GLSL allows shorthand use of vectors too, the following is also valid:
    // gl_Position = vec4(in_Position, 0.0, 1.0);

    // Pass the color through unmodified
    ex_Colour = in_Colour;
    }
