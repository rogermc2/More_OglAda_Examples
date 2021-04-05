
#version 410 core

out vec4 frag_colour;

in vec2 fp, st;
uniform sampler2D tex;
uniform float t;
uniform float f;

void main ()
    {
	vec2 ast;
	// put in range of -1 to 1
	ast = st * 2.0 - 1.0;
	ast.s += sin (t * 3.0 + (ast.t + 1.0) * f * 8.0) * 0.05 * f;
	// back in range of 0 to 1
	ast = (ast + 1.0) * 0.5;
	
	frag_colour = texture (tex, ast);
    }
