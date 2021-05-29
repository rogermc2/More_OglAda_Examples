#version 410 core

layout (location = 0) in vec3 points3d;
layout (location = 1) in vec3 normals;
layout (location = 1) in vec2 uvs_in;

uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

out vec2 uvs;

void main()
    {
//    gl_PointSize = 40.0;
    gl_Position = projection_matrix * view_matrix * model_matrix * vec4(points3d, 1.0);
    uvs = uvs_in;
    }
