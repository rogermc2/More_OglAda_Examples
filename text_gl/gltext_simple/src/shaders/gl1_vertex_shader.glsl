#version 410 core

layout(location = 0) in vec4 vertices;  // <vec2 position, vec2 texture>

out vec2 texture_coords;

uniform mat4 model_view_matrix;
uniform mat4 projection_matrix;

void main()
{
    gl_Position = projection_matrix * model_view_matrix * vec4(vertices.xy, 0.0, 1.0);
    texture_coords = vec2(vertices.zw);
}
