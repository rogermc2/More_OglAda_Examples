#version 410 core

in vec2 position;
in vec3 colour;
in vec2 texcoord;

uniform mat4 trans;
uniform mat4 view;
uniform mat4 proj;

out vec3 v_colour;
out vec2 v_texcoord;

void main()
    {
    v_colour = colour;
    v_texcoord = texcoord;
    gl_Position = proj * view * trans * vec4(position, 0.0, 1.0);
    }
