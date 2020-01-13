#version 410 core

// blinn shading with texture

layout(location = 0) in vec3 Position;
layout(location = 1) in vec2 TexCoord;
layout(location = 2) in vec3 Normal;

out vec3 esVertex;
out vec3 esNormal;
out vec2 texCoord0;

uniform mat4 matrixModelView;
uniform mat4 matrixNormal;
uniform mat4 matrixModelViewProjection;

void main()
    {
    esVertex = vec3(matrixModelView * vec4(Position, 1.0));
    esNormal = vec3(matrixNormal * vec4(Normal, 1.0));
    texCoord0 = TexCoord;
    gl_Position = matrixModelViewProjection * vec4(Position, 1.0);
    }

