#version 410 core

// the fragment shader creates a bell like radial color distribution 
layout(location = 0) out vec4 FragColor;

in vec2 billboardcoord;

void main()
   {
   float s = 0.2 * (1 / (1 + 15.*dot(billboardcoord, billboardcoord)) -1/16.);
   FragColor = s*vec4(0.3, 0.3, 1.0, 1);
   }
