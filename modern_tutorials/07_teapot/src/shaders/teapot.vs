#version 410 core

layout (location = 0) in vec3 coord3d;
layout (location = 1) in vec3 v_color;

uniform mat4 mvp;

out vec3 f_color;

void main(void)
  {
  gl_Position = mvp * vec4(coord3d, 1.0);
  f_color = v_color;
  }
