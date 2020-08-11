#version 410 core
layout (triangles) in;
layout(triangle_strip, max_vertices = 6) out;

flat in vec3 geom_Colour [3];
flat out vec3 ex_Colour;

uniform mat4 mvp_matrix;

void main()
    {
    int index;
    for (index = 0; index < gl_in.length(); index++)
        {
         gl_Position = mvp_matrix * gl_in[index].gl_Position;
         ex_Colour = geom_Colour[index];
         EmitVertex();
        }
    }
