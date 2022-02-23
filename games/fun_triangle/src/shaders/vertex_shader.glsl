#version 410

layout (location = 0) in vec2 points2d;
layout (location = 1) in vec3 colour;

out vec3 colours = colour;

void main()
{
  gl_PointSize = 10.0;
  gl_Position = vec4(points2d, 0.0, 1.0);
}
