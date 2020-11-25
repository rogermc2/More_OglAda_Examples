#version 410 core

layout (location = 0) vec3 coord3d;
layout (location = 1) vec2 texcoord;

out vec2 f_texcoord;
uniform mat4 mvp;

void main(void)
  {
  gl_Position = mvp * vec4(coord3d, 1.0);
  f_texcoord = texcoord;
  }
