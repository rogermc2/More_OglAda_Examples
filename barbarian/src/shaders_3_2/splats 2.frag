//
// Crongdor the Barbarian
// blood splatters that stick to walls
// First version Anton Gerdelan, 31 Jan 2014
// Latest code review 5 Feb 2015
//

#version 150
out vec4 frag_colour;

// vertex points in light coordinate space
in vec3 texcoords; // TODO use p_wor instead
in vec2 st;
in vec3 p_eye; // TODO use p_wor instead
in vec3 n_eye;
uniform sampler2D tex;

uniform mat4 V;

// dynamic scene light
uniform vec3 dyn_light_pos_wor;
uniform vec3 dyn_light_diff;
uniform vec3 dyn_light_spec;
uniform float dyn_light_range;
uniform vec3 L_a; // ambient light colour

// shadow
uniform float shadow_enabled;
uniform samplerCube cube_texture;
uniform vec3 caster_pos_wor;

//float light_emission_factor = 1.0;
float specular_exponent = 50.0;

// work out contribution of an individual light
void calc_a_light (
	in vec3 light_pos_wor, in float range, in vec3 l_d, in vec3 l_s,
	in vec4 texel_diff, in vec4 texel_spec, in vec3 n_eye,
	inout vec3 I_d, inout vec3 I_s
) {
	vec3 light_pos_eye = (V * vec4 (light_pos_wor, 1.0)).xyz;
	float d = distance (light_pos_eye, p_eye);
	float roll_off_fac = range - clamp (d, 0.0, range); // 1 to 0
	roll_off_fac = clamp (floor (roll_off_fac * 3.0) / 3.0, 0.0, 1.0);
	// phong diffuse
	vec3 dir = normalize (light_pos_eye - p_eye);
	float diff_fac = max (dot (dir, n_eye), 0.0);
	// blinn-phong specular
	vec3 v = normalize (-p_eye);
	vec3 half_way_eye = normalize (v + dir);
	float dp_spec = max (dot (half_way_eye, n_eye), 0.0);
	float spec_fac = pow (dp_spec, specular_exponent);
	// summations
	I_d += l_d * texel_diff.rgb * diff_fac * roll_off_fac;
	I_s += l_s * texel_spec.rgb * spec_fac * roll_off_fac;
}

// work out all lighting on surface
vec3 blinn_phong (in vec4 texel_diff, in vec4 texel_spec) {
	vec3 n = normalize (n_eye); // re-normalised normal
	vec3 I_a = texel_diff.rgb * L_a; // summed ambient intensity
	vec3 I_d = vec3 (0.0, 0.0, 0.0); // summed diffuse intensity
	vec3 I_s = vec3 (0.0, 0.0, 0.0); // summed specular intensity
	// dynamic light
	calc_a_light (
		dyn_light_pos_wor, dyn_light_range,
		dyn_light_diff, dyn_light_spec,
		texel_diff, texel_spec, n,
		I_d, I_s
	);
	// summation
	return I_a + I_d + I_s;
}

float eval_shadow (in float l, in vec3 tc, in samplerCube cube) {
	float texel = texture (cube, tc).r;
	// bias is used in case of self-shadowing
	float bias = 0.025;
	float diff = (texel + bias) - l;
	if (diff < 0.0) {
		return 0.01;
	}
	return 1.0;
}

void main () {
	vec4 texel_diff = texture (tex, st);
	vec4 spec_colour = vec4 (0.1, 0.1, 0.1, 1.0);
	frag_colour.a = texel_diff.a;
	// lights
	vec3 I = blinn_phong (texel_diff, spec_colour);
	frag_colour.rgb = I;
	// shadows
	if (shadow_enabled > 0.0) {
		vec3 dir = texcoords - caster_pos_wor;
		float l = length (dir);
		float sf_f = eval_shadow (l, dir, cube_texture);
		//sf_f = mix (sf_f, 1.0, 1.0 - light_emission_factor);
		frag_colour.rgb *= sf_f;
	}
}
