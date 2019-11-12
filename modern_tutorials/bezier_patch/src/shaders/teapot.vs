#version 410 core

layout (location = 0) in vec3 coord3d;

uniform mat4 mvp;

void main(void)
  {
  gl_PointSize = 10.0;
  gl_Position = mvp * vec4(coord3d, 1.0);
  }
