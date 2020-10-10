
#version 410 core

out vec4 frag_colour;

in vec2 st;
in vec3 n, p;

uniform sampler2D dm;
uniform float time;
uniform float ol_pass;

void main ()
    {
	if (ol_pass > 0.1)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
		return;
        }
	// shading darker based on angle to eye
	vec3 n_eye = normalize (n);
	vec3 p_to_eye = normalize (-p);
	float dp = dot (n_eye, p_to_eye);
	// reduce to 0.0 to 0.8
	dp *= 0.25;
	
	vec4 texel = texture (dm, st);
	
	// shimmer of gold
	float shimmer = sin (time * 4.0) * 0.5 + 0.5;
	vec3 colour = texel.rgb * max (dp, shimmer);
	frag_colour = vec4 (colour, texel.a);
    }
