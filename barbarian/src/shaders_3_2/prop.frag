//
// Crongdor the Barbarian
// 3d prop shader
// First version Anton Gerdelan, 31 Jan 2014
// Latest code review 9 Mar 2016
// -- managed to reduce in floats from 20 to 12!
// (didnt make fps difference on amd at work but is under apple's reported max)
//

#version 410 Core
out vec4 frag_colour;
#define NUM_LIGHTS 3

// vertex points in light coordinate space
in vec2 st;
in vec3 p_wor;
in vec3 ftangent;
in vec3 fnormal;

uniform mat4 P, V;

// dynamic scene light
uniform vec3 dyn_light_pos_wor;
uniform vec3 dyn_light_diff;
uniform vec3 dyn_light_spec;
uniform float dyn_light_range;

// static scene lights
uniform vec3 L_a;
uniform ivec2 static_light_indices;
uniform vec3 light_pos[32];
uniform vec3 light_diff[32];
uniform vec3 light_spec[32];
uniform float light_range[32];

uniform sampler2D diff_map, spec_map;

// normal mapping
uniform sampler2D norm_map;
uniform mat4 inv_M;
uniform vec3 cam_pos_wor;

// shadow
uniform float shadow_enabled;
uniform samplerCube cube_texture;
uniform vec3 caster_pos_wor;

uniform float ol_pass;

float specular_exponent = 50.0;
vec3 view_dir_tan;
vec3 light_dir_tan[NUM_LIGHTS];

void calc_normal_map_vars ()
    {
	vec3 nftan = normalize (ftangent);
	vec3 nfnormal = normalize (fnormal);
	vec3 bitangent = cross (nfnormal, nftan);
	vec3 cam_pos_loc = vec3 (inv_M * vec4 (cam_pos_wor, 1.0));
	vec3 p_loc = vec3 (inv_M * vec4 (p_wor, 1.0));
	vec3 view_dir_loc = normalize (cam_pos_loc - p_loc);
	view_dir_tan = vec3 (
		dot (nftan, view_dir_loc),
		dot (bitangent, view_dir_loc),
		dot (nfnormal, view_dir_loc)
	);
	
	// lights into tangent space
	vec3 L_pos_wor, L_dir_wor, L_dir_loc;
	
	L_pos_wor = light_pos[static_light_indices.x];
	L_dir_wor = normalize (p_wor - L_pos_wor);
	L_dir_loc = vec3 (inv_M * vec4 (L_dir_wor, 0.0));
	light_dir_tan[0] = vec3 (
		dot (nftan, L_dir_loc),
		dot (bitangent, L_dir_loc),
		dot (nfnormal, L_dir_loc)
	);
	
	L_pos_wor = light_pos[static_light_indices.y];
	L_dir_wor = normalize (p_wor - L_pos_wor);
	L_dir_loc = vec3 (inv_M * vec4 (L_dir_wor, 0.0));
	light_dir_tan[1] = vec3 (
		dot (nftan, L_dir_loc),
		dot (bitangent, L_dir_loc),
		dot (nfnormal, L_dir_loc)
	);
	
	L_pos_wor = dyn_light_pos_wor;
	L_dir_wor = normalize (p_wor - L_pos_wor);
	L_dir_loc = vec3 (inv_M * vec4 (L_dir_wor, 0.0));
	light_dir_tan[2] = vec3 (
		dot (nftan, L_dir_loc),
		dot (bitangent, L_dir_loc),
		dot (nfnormal, L_dir_loc)
	);
    }

// work out contribution of an individual light
void calc_a_light (
	in vec3 light_pos_wor, in float range, in vec3 l_d, in vec3 l_s,
	in vec3 l_dir_tan, in vec3 view_dir_tan_n,
	in vec4 texel_diff, in vec4 texel_spec, in vec3 n_tan,
	inout vec3 I_d, inout vec3 I_s
)
    {
	float d = distance (light_pos_wor, p_wor);
	float roll_off_fac = range - clamp (d, 0.0, range); // 1 to 0
	roll_off_fac = clamp (floor (roll_off_fac * 3.0) / 3.0, 0.0, 1.0);
	// phong diffuse
	vec3 light_dir_tan_n = normalize (l_dir_tan); // direction to light
	float diff_fac = clamp (dot (-light_dir_tan_n, n_tan), 0.0, 1.0);
	// blinn specular
	vec3 half_way_tan = normalize (view_dir_tan_n + -light_dir_tan_n);
	float dp_spec = clamp (dot (half_way_tan, n_tan), 0.0, 1.0);
	float spec_fac = pow (dp_spec, specular_exponent);
	spec_fac = clamp (spec_fac, 0.0, 1.0);
	// summations
	I_d += l_d * texel_diff.rgb * diff_fac * roll_off_fac;
	I_s += l_s * texel_spec.rgb * spec_fac * roll_off_fac;
    }

// work out all lighting on surface
vec3 blinn_phong (in vec4 texel_diff, in vec4 texel_spec, in vec3 n_tan)
    {
	vec3 I_a = texel_diff.rgb * L_a; // summed ambient intensity
	vec3 I_d = vec3 (0.0, 0.0, 0.0); // summed diffuse intensity
	vec3 I_s = vec3 (0.0, 0.0, 0.0); // summed specular intensity
	vec3 view_dir_tan_n = normalize (view_dir_tan);
	
	// static light #1
	vec3 l_dir_tan = light_dir_tan[0];
	calc_a_light (
		light_pos[static_light_indices.x], light_range[static_light_indices.x],
		light_diff[static_light_indices.x], light_spec[static_light_indices.x],
		l_dir_tan, view_dir_tan_n,
		texel_diff, texel_spec, n_tan,
		I_d, I_s
	);
	// static light #2
	l_dir_tan = light_dir_tan[1];
	calc_a_light (
		light_pos[static_light_indices.y], light_range[static_light_indices.y],
		light_diff[static_light_indices.y], light_spec[static_light_indices.y],
		l_dir_tan, view_dir_tan_n,
		texel_diff, texel_spec, n_tan,
		I_d, I_s
	);
	// dynamic light
	l_dir_tan = light_dir_tan[2];
	calc_a_light (
		dyn_light_pos_wor, dyn_light_range,
		dyn_light_diff, dyn_light_spec,
		l_dir_tan, view_dir_tan_n,
		texel_diff, texel_spec, n_tan,
		I_d, I_s
	);
	// summation
	return I_a + I_d + I_s;
    }

float eval_shadow (in float l, in vec3 tc, in samplerCube cube)
    {
	float texel = texture (cube, tc).r;
	// bias is used in case of self-shadowing
	float bias = 0.025;
	float diff = (texel + bias) - l;
	if (diff < 0.0)
        {
		return 0.01;
        }
	return 1.0;
    }

void main ()
    {
	if (ol_pass > 0.1)
        {
		frag_colour = vec4 (0.0, 0.0, 0.0, 1.0);
		return;
        }
	// sampling
	vec4 texel_diff = texture (diff_map, st);
	vec4 texel_spec = texture (spec_map, st);
	vec3 n_tan = texture (norm_map, st).rgb;
	n_tan = normalize (n_tan * 2.0 - 1.0);
	frag_colour.a = texel_diff.a;
	float light_emission_factor = texel_spec.a;
	// lights
	calc_normal_map_vars ();
	vec3 I = blinn_phong (texel_diff, texel_spec, n_tan);
	frag_colour.rgb = mix (I, texel_diff.rgb, 1.0 - light_emission_factor);
	if (shadow_enabled > 0.0)
        {
		vec3 dir = p_wor - caster_pos_wor;
		float l = length (dir);
		float sf_f = eval_shadow (l, dir, cube_texture);
		// shadow affected by emission map
		sf_f = mix (sf_f, 1.0, 1.0 - light_emission_factor);
		frag_colour.rgb *= sf_f;
        }
    }
