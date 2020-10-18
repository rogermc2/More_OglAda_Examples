
#version 410 core

out vec4 frag_colour;

in vec3 p_loc;
in vec3 n_loc;

vec3 top_c = vec3 (0.0, 1.0, 0.0);
vec3 mid_c = vec3 (1.0, 1.0, 1.0);
vec3 bot_c = vec3 (0.0, 0.0, 1.0);

uniform float time;

void main ()
    {
	// title is 0 to 3 on vertical
	float top_f = max (0.0, p_loc.y - 2.0);
	
	// 0 to 0.5 shd be the bottom highlight range
	// 0.75   0
	// 0.5    0.5
	// 0.0    1.0
	// max (0.0, -(y - 0.75))
	float bot_f = max (0.0, -(p_loc.y - 0.75)) / 0.75;
	
	// 0.5 to 1.5, so dist of y from middle point: 1.5
	float midp = 1.5;
	float dist = abs (midp - p_loc.y);
	float max_dist = 1.3;
	float dist_f = clamp (dist, 0.0, max_dist) / max_dist;
	float mid_f = 1.0 - dist_f;
	
	vec3 mix_c = top_c * top_f + bot_c * bot_f + mid_c * mid_f;
	
	vec3 light = normalize (vec3 (0.0, 0.05, -1.0));
	float dp = max (0.0, dot (normalize (-n_loc), light));
	frag_colour.rgb = mix_c * dp * 0.99 + mix_c * 0.01;
	
	float peakx = -5.0 + mod (time * 20.0, 20.0);
	float peakd = max (0.0, 1.0 - abs (peakx - p_loc.x));
	float shinef = max(0.1,mid_f)* peakd;
	frag_colour.rgb = frag_colour.rgb + vec3 (shinef,shinef,shinef);
	
	frag_colour.a = 1.0;
    frag_colour = vec4(1.0, 0.0, 0.0, 1.0);
    }
