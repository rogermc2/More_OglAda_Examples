
// 3d menu cursor

#version 410 core

out vec4 frag_colour;

in vec3 p_eye;
in vec3 n_eye;
in vec2 st;

uniform sampler2D diff_map;

void main ()
    {
	vec4 texel_diff = texture (diff_map, st);
	vec3 n = normalize (n_eye);
	float dp = max (0.0, dot (n, normalize (-p_eye)));
	frag_colour.rgb = 3.0 * texel_diff.rgb * dp;
	frag_colour.a = texel_diff.a;
    }
