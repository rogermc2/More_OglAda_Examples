#version 410 core

in vec2 v_texcoord;

uniform sampler2D texture2D;

out vec4 fragment_colour;

void main()
    {
    vec2 flipped_texcoord = vec2(f_texcoord.x, 1.0 - f_texcoord.y);
    fragment_colour = texture(texture2D, flipped_texcoord);
    }
