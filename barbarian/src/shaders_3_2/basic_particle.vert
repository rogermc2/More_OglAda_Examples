//
// Crongdor the Barbarian
// basic point-sprite particle shader
// First version Anton Gerdelan, 4 Aug 2013
// Latest code review 5 Dec 2014
//
// ANDREA: changed from 'degrees' to 'degs' because name was reserved GL 4.1
//

#version 410 core

in vec3 particle_world_pos;
in float particle_age;

uniform mat4 PV;
uniform vec4 initial_colour, final_colour;

uniform float initial_scale, final_scale, degs, lifetime, width_px;

// this dummy variable is some kind of hack to trick an Apple mix-up outs
// bug
//out vec4 dummy;
out vec4 fcolour;
out float fdegree;

void main ()
    {
	//dummy = vec4 (1.0, 1.0, 1.0, 1.0);
	float life_fac = particle_age / lifetime;
	fcolour = life_fac * final_colour + (1.0 - life_fac) * initial_colour;

	// fade-in
	if (life_fac < 0.1) {
		fcolour.a *= max (0.0, life_fac * 10.0);
	}
	// fade-out
	if (life_fac > 0.9) {
		fcolour.a *= min (1.0, (1.0 - life_fac) * 10.0);
	}

	fdegree = degs * particle_age;
	gl_Position = PV * vec4 (particle_world_pos, 1.0);

	// scale the px size here
	float curr_scale = life_fac * final_scale + (1.0 - life_fac) * initial_scale;
	vec4 pp = PV *
		vec4 (
			particle_world_pos.x + initial_scale,
			particle_world_pos.y,
			particle_world_pos.z,
			1.0
		);
	float dist = 2.0 * abs (gl_Position.x / gl_Position.w - pp.x / pp.w);

	gl_PointSize = width_px * curr_scale * dist;
}
