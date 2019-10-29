#version 410 core

// This input vector contains the vertex position in xyz and the
// mass of the vertex in w
layout (location = 0) in vec4 position_mass;
// This is the current velocity of the vertex
layout (location = 1) in vec3 velocity;
// This is our connection vector
layout (location = 2) in ivec4 connection;

// This is a TBO that will be bound to the same buffer as the
// position_mass input attribute
uniform samplerBuffer tex_position;

// The outputs of the vertex shader are the same as the inputs
out vec4 tf_position_mass;
out vec3 tf_velocity;

uniform float time_step = 0.07;
uniform float spring_constant = 7.1;

// Gravity
const vec3 gravity = vec3(0.0, -0.08, 0.0);

// Global damping constant
uniform float c = 2.8;
uniform float rest_length = 0.88;

void main(void)
{
    vec3 p = position_mass.xyz;    // p can be our position
    float mass = position_mass.w;     // m is the mass of our vertex
    vec3 u = velocity;             // u is the initial velocity
    vec3 force = gravity * mass - c * u;  // F is the force on the mass
    bool fixed_node = true;        // Becomes false when force is applied

    for (int i = 0; i < 4; i++)
        {
        if (connection[i] != -1)
            {
            // q is the position of the other vertex
            vec3 q = texelFetch(tex_position, connection[i]).xyz;
            vec3 d = q - p;
            float x = length(d);
            force = force -spring_constant * (rest_length - x) * normalize(d);
            fixed_node = false;
            }
        }

    // If this is a fixed node, reset force to zero
    if (fixed_node)
        {
        force = vec3(0.0);
        }

    // Accelleration due to force
    vec3 acc = force / mass;

    // Displacement
    vec3 s = u * time_step + 0.5 * acc * time_step * time_step;

    // Final velocity
    vec3 v = u + acc * time_step;

    // Constrain the absolute value of the displacement per step
    s = clamp(s, vec3(-25.0), vec3(25.0));

    // Write the outputs
    tf_position_mass = vec4(p + s, m);
    tf_velocity = v;
}
