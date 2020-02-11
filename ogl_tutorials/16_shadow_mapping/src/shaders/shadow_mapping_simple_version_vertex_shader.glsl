#version 410 core

layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;
layout(location = 2) in vec3 vertexNormal_modelspace;

out vec2 UV;
out vec4 ShadowCoord;

uniform mat4 MVP;
uniform mat4 DepthBiasMVP;

void main()
    {
    //  gl_Position is the position of the vertex as seen from the current camera.
    //  ShadowCoord is the position of the vertex as seen from the last camera (the light)
	gl_Position =  MVP * vec4(vertexPosition_modelspace,1);
	ShadowCoord = DepthBiasMVP * vec4(vertexPosition_modelspace,1);
	UV = vertexUV;
    }

