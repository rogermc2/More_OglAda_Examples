//
// Crongdor the Barbarian
// flash gold multi-pass shader
// First version Anton Gerdelan, ? 2013
// Latest code review 15 Dec 2014
//

#version 150
out vec4 frag_colour;

in vec2 st;
uniform float t;
uniform sampler2D tex;

void main () {
	vec3 scene = texture (tex, st).rgb;
	vec3 gold = vec3 (1.0, 0.5, 0.0);
	frag_colour = vec4 (mix (scene, gold, abs (sin (t * 6.28 * 2.0) * 0.15)), 1.0);
}
