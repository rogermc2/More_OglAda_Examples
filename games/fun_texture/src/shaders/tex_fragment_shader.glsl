#version 410 core

in vec2 v_texcoord;

uniform sampler2D texture2d;

out vec4 fragment_colour;

void main()
    {
    vec2 flipped_texcoord = vec2(v_texcoord.x, 1.0 - v_texcoord.y);
    fragment_colour = texture(texture2d, flipped_texcoord);
    }
