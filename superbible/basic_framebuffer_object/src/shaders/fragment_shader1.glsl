#version 410 core

 in VS_OUT
{
    vec4 colour;
    vec2 texcoord;
} fs_in;

 out vec4 fragment_colour;

void main()
{
    fragment_colour = 0.5*sin(fs_in.colour * vec4(40.0, 20.0, 30.0, 1.0)) + vec4(0.5);
}
