
#version 410 core

out vec4 frag_colour;

in vec2 st;
uniform float t;
uniform sampler2D tex;

void main ()
    {
	vec3 scene = texture (tex, st).rgb;
	vec3 white = vec3 (0.8, 0.8, 1.0);
//	frag_colour = vec4 (mix (scene, white, abs (sin (t * 6.28 * 12.0) * 0.75)), 1.0);
	float f = 1.0 - t / 1.0;
	frag_colour = vec4 (mix (scene, white, f), 1.0);
    }
