
#version 410 core

out vec4 frag_colour;

in vec2 st;
uniform float t;
uniform sampler2D tex;

void main ()
    {
	vec3 scene = texture (tex, st).rgb;
	vec3 gold = vec3 (1.0, 0.0, 0.0);
	frag_colour = vec4 (mix (scene, gold, abs (sin (t * 6.28 * 3.0) * 0.1)), 1.0);
    }
