#version 410 core

uniform sampler2D tex;
out vec4 fragment_colour;

 in VS_OUT
{
    vec4 colour;
    vec2 texcoord;
} fs_in;

void main()
{
   fragment_colour = mix(fs_in.colour, texture(tex, fs_in.texcoord), 0.7);
}
