//
// Crongdor the Barbarian
// wobbly portal shader
// First version Anton Gerdelan, 17 Jan 2015
// Latest code review
//
#version 410 core
out vec4 frag_colour;

in vec2 st;
in vec3 p;
uniform sampler2D dm;
uniform float time;

void main ()
    {
	// x -1.3 to 1.3
	// y 0 to 4.1
	vec2 ast;
	ast.s = st.s + sin (time * 4.0 + p.y * 2.0) * 0.1;
	ast.t = st.t + cos (time * 2.0 + p.x * 2.0) * 0.1;
	vec4 texel = texture (dm, ast);
	
	frag_colour = vec4 (texel.rgb, 0.9);
    }
