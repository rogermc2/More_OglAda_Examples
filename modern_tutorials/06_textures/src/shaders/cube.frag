#version 410 core

in vec2 f_texcoord;

out vec4 FragColour;

uniform sampler2D mytexture;

void main(void)
  {
  vec2 flipped_texcoord = vec2(f_texcoord.x, 1.0 - f_texcoord.y);
  FragColour = texture(mytexture, flipped_texcoord);
  }
