#version 410 core

uniform vec3 f_color;

out vec4 FragColor;

void main(void)
  {
  FragColor = vec4(f_color.xyz, 1.0);
  }
