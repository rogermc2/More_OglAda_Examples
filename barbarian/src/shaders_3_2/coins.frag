#version 410 core

out vec4 frag_colour;

in vec2 st;
in vec3 n, p; // TODO use p_wor instead
// shadows
//in vec4 st_shadow[6];
in vec3 texcoords; // TODO use p_wor instead

uniform sampler2D dm;
uniform float time;
uniform float ol_pass;

// shadow
//uniform sampler2D depth_map[6];
uniform float shadow_enabled;
//uniform float epsilon;
uniform samplerCube cube_texture;
uniform vec3 caster_pos_wor;

float eval_shadow (in float l, in vec3 tc, in samplerCube cube) {
	float texel = texture (cube, tc).r;
	// bias is used in case of self-shadowing
	float bias = 0.025;
	float diff = (texel + bias) - l;
	if (diff < 0.0) {
		return 0.0;
	}
	return 1.0;
}

void main () {
	if (ol_pass > 0.1) {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
		return;
	}
	// shading darker based on angle to eye
	vec3 n_eye = normalize (n);
	vec3 p_to_eye = normalize (-p);
	float dp = dot (n_eye, p_to_eye);
	// reduce to 0.0 to 0.8
	dp *= 0.6;
	
	vec4 texel = texture (dm, st);
	// shimmer of gold
	float shimmer = max (sin (time * 7.5 + p.x * 2.0), 0.0);
	vec3 colour = texel.rgb * max (dp, shimmer);

	frag_colour = vec4 (colour, 1.0);
	if (shadow_enabled > 0.0) {
		vec3 dir = texcoords - caster_pos_wor;
		float l = length (dir);
		float sf_f = eval_shadow (l, dir, cube_texture);
		// shadow affected by emission map
		//sf_f = mix (sf_f, 1.0, 1.0 - light_emission_factor);
		frag_colour.rgb *= sf_f;
	}
}
