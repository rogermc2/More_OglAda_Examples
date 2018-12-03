#version 410 core

layout (location = 0) in vec3 Position;
layout (location = 1) in vec2 TexCoord;
layout (location = 2) in vec3 Normal;

uniform mat4 gWVP;

out vec2 TexCoordOut;

void main()                                                                         
{
   gl_PointSize = 40.0;
//    gl_Position = gWVP * vec4(Position, 1.0);
    gl_Position = vec4(0.0,0.0, 1.0, 1.0);
   TexCoordOut = TexCoord;
}
