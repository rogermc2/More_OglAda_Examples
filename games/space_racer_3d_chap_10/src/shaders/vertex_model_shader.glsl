#version 410 core

layout (location = 0) in vec3 points3d;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

uniform vec3 colour_in;

out vec3 frag_colour;

void main()
    {
    gl_Position = projection_matrix * view_matrix * model_matrix * vec4(points3d, 1.0);
    frag_colour = colour_in;
    }
