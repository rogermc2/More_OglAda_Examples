#version 410 core

layout (location = 0) out vec4 colour;

uniform sampler2D fur_texture;
uniform vec4 fur_colour;

in GS_FS_INTERFACE
{
    vec3 normal;
    vec2 tex_coord;
    flat float fur_strength;
} fragment_in;

void main(void)
{
    vec4 rgba = texture(fur_texture, fragment_in.tex_coord);
    float t = rgba.a;
    t = t * fragment_in.fur_strength;
    colour = fur_colour * vec4(1.0, 1.0, 1.0, t);
}
