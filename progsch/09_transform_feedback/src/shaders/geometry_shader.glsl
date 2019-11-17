#version 410 core

// the geometry shader creates the billboard quads
layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

uniform mat4 View;
uniform mat4 Projection;

out vec2 billboardcoord;

void main()
   {
   vec4 pos = View * gl_in[0].gl_Position;
       
   billboardcoord = vec2(-1, -1);
   gl_Position = Projection * (pos + 0.2 * vec4(billboardcoord, 0, 0));
   EmitVertex();
    
   billboardcoord = vec2( 1, -1);
   gl_Position = Projection * (pos + 0.2 * vec4(billboardcoord, 0, 0));
   EmitVertex();

   billboardcoord = vec2(-1, 1);
   gl_Position = Projection * (pos + 0.2 * vec4(billboardcoord, 0, 0));
   EmitVertex();

   billboardcoord = vec2( 1, 1);
   gl_Position = Projection * (pos + 0.2 * vec4(billboardcoord, 0, 0));
   EmitVertex();
   }
