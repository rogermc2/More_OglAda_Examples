//
// Crongdor the Barbarian
// water tile shader
// First version Anton Gerdelan, ? 2014
// Latest code review 5 Feb 2016
//

#version 150

in vec3 vp;

uniform mat4 M, V, P;
uniform float t; // time. used for vertex animation
out vec3 p_eye, p_wor, n_eye;

float two_pi = 6.28319; // 2pi
int numWaves = 4;
// doubled these after fixing normal derivatives (apg - 25 dec 2015)
float amplitude[4] = float[] (0.12, 0.08, 0.04, 0.02);
float wavelength[4] = float[] (1.0, 0.7, 0.4, 0.6);
float speed[4] = float[] (0.4, 0.4, 0.3, 0.2);
// I pre-normalised these to 3sf
vec2 direction[4] = vec2[] (
	vec2 (0.873, 0.489),
	vec2 (1.0, 0.0),
	vec2 (0.981, -0.196),
	vec2 (0.4, 0.2)
);

void wave (in int i, in float x, in float y,
	inout float out_y, inout vec3 out_n) {
	float frequency = two_pi / wavelength[i];
	float phase = speed[i] * frequency;
	float theta = dot (direction[i], vec2 (x, y));
	float height = amplitude[i] * sin (theta * frequency + t * phase);
	// note: n is not normalised here - do so in frag shader
	vec2 n;
	n.x = amplitude[i] * cos (theta * frequency + t * phase);
	n.y = 1.0 - abs (n.x);
	// put in direction of wave also
	out_n += vec3 (n.x * direction[i].x, n.y, n.x * direction[i].y);
	out_y += height;
}

void waveHeight (in float x, in float y, out float h, out vec3 n) {
	h = 0.0;
	n = vec3 (0.0, 0.0, 0.0);
	wave (0, x, y, h, n);
	wave (1, x, y, h, n);
	wave (2, x, y, h, n);
	wave (3, x, y, h, n);
}

void main () {
	p_wor = vec3 (M * vec4 (vp, 1.0));
	float wave_h;
	vec3 wave_n;
	waveHeight (p_wor.x, p_wor.z, wave_h, wave_n);
	p_wor.y += wave_h - 0.5;
	p_eye = (V * vec4 (p_wor, 1.0)).xyz;
	n_eye = vec3 (V * M * vec4 (wave_n, 0.0));
	gl_Position = P * V * vec4 (p_wor, 1.0);
}
