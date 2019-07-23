#version 410 core

layout (location = 0) in vec3 Position;

uniform mat4 gWVP;

out vec4 Colour;

void main()
{
    gl_Position = gWVP * vec4(Position, 1.0);
    Colour = vec4(clamp(Position, 0.0, 1.0), 1.0);
}
