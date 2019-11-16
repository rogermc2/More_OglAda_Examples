#version 410 core

layout(location = 0) out vec4 FragColor;

in vec2 txcoord;

void main()
   {
   float s = 0.2 * (1 / (1 + 15.*dot(txcoord, txcoord)) -1/16.);
   FragColor = s*vec4(0.3, 0.3, 1.0, 1);
   }
