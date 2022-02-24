#version 410 core

flat in  vec3 ex_Colour;
out vec4 frag_Colour;

void main(void)
    {
    frag_Colour = vec4(ex_Colour, 1.0);
    }
