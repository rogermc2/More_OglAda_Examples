#version 410 core

// The position_mass vector contains the vertex position in xyz
// and the mass of the vertex in w
layout (location = 0) in vec4 position_mass;
layout (location = 1) in vec3 velocity;
layout (location = 2) in ivec4 connection;

// samplerBuffer is the OpenGL Texture_Buffer to which is bound the same buffer
// as the position_mass input attributeis bound
uniform samplerBuffer tex_position;

out vec4 tf_position_mass;
out vec3 tf_velocity;

uniform float time_step;
uniform float spring_constant = 7.1;

const vec3 gravity = vec3(0.0, -0.08, 0.0);

uniform float damping_constant = 2.8;
uniform float rest_length = 0.88;

void main(void)
    {
    vec3 vertex_pos = position_mass.xyz;
    float mass = position_mass.w;
    vec3 initial_velocity = velocity;
    vec3 delta_pos;
    vec3 displacement;
    vec3 acc;
    vec3 final_vel;
    vec3 force = gravity * mass - damping_constant * initial_velocity;

    for (int i = 0; i < 4; i++)
        {
        if (connection[i] != -1)
            {
            delta_pos = texelFetch(tex_position, connection[i]).xyz - vertex_pos;
            force = force + spring_constant * (length(delta_pos) - rest_length) * normalize(delta_pos);
            }
        }
 
    acc = force / mass;
    displacement = initial_velocity * time_step + 0.5 * acc * time_step * time_step;
    final_vel = initial_velocity + acc * time_step;

    // Constrain the absolute value of the displacement per step
    displacement = clamp(displacement, vec3(-25.0), vec3(25.0));   //  25.0

    tf_position_mass = vec4(vertex_pos + displacement, mass);
    tf_velocity = final_vel;
    }
