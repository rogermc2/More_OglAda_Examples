#version 410 core

// layout(location = 0) in vec4 vposition;
layout(location = 0) in vec3 inposition;
layout(location = 1) in vec3 invelocity;

uniform vec3 center[3];  // Sphere centres
uniform float radius[3]; // Sphere radii
uniform vec3 g;
uniform float dt;
uniform float bounce;
uniform int seed;

out vec3 outposition;
out vec3 outvelocity;

float hash(int x)
   {
   x = x * 1235167 + gl_VertexID * 948737 + seed * 9284365;
   x = (x >> 13) ^ x;
   return ((x * (x * x * 60493 + 19990303) + 1376312589) & 0x7fffffff) / float(0x7fffffff - 1);
   }

void main()
   {
   outvelocity = invelocity;
   for (int j = 0; j < 3; ++j)
       {
       vec3 diff = inposition - center[j];
       float dist = length(diff);
       float vdot = dot(diff, invelocity);
       if (dist < radius[j] && vdot < 0.0)
           outvelocity = outvelocity - bounce * diff * vdot / (dist * dist);
       }
   outvelocity = outvelocity + dt * g;
   outposition = inposition + dt * outvelocity;

  if (outposition.y < -30.0)
      {
       outvelocity = vec3(0, 0, 0);
       outposition = 0.5 - vec3(hash(3 * gl_VertexID + 0), hash(3 * gl_VertexID + 1), hash(3 * gl_VertexID + 2));
       outposition = vec3(0, -20, 0) + 5.0 * outposition;
      }
  }
