#version 410 core

//  #extension GL_EXT_gpu_shader4 : enable
//  #extension GL_EXT_geometry_shader4: enable

layout(points) in;
layout(triangle_strip) out;
layout(max_vertices = 4) out;

const int maxVerticesOut = 4;
//GL_GEOMETRY_INPUT_TYPE_EXT = GL_POINTS;
//GL_GEOMETRY_OUTPUT_TYPE_EXT = GL_TRIANGLE_STRIP;
//GL_GEOMETRY_VERTICES_OUT_EXT = 4;

uniform mat4 gVP;                                                                   
uniform vec3 gCameraPos;                                                            
                                                                                    
out vec2 TexCoord;
                                                                                    
void main()                                                                         
{                                                                                   
    vec3 Pos = gl_Position.xyz;                                            
    vec3 toCamera = normalize(gCameraPos - Pos);                                    
    vec3 up = vec3(0.0, 1.0, 0.0);                                                  
    vec3 right = cross(toCamera, up);                                               
                                                                                    
    Pos = Pos - (right * 0.5);
    gl_Position = gVP * vec4(Pos, 1.0);                                             
    TexCoord = vec2(0.0, 0.0);                                                      
    EmitVertex();                                                                   
                                                                                    
    Pos.y = Pos.y + 1.0;
    gl_Position = gVP * vec4(Pos, 1.0);                                             
    TexCoord = vec2(0.0, 1.0);                                                      
    EmitVertex();                                                                   
                                                                                    
    Pos.y = Pos.y - 1.0;
    Pos = Pos + right;
    gl_Position = gVP * vec4(Pos, 1.0);                                             
    TexCoord = vec2(1.0, 0.0);                                                      
    EmitVertex();                                                                   
                                                                                    
    Pos.y = Pos.y + 1.0;                                                                   
    gl_Position = gVP * vec4(Pos, 1.0);                                             
    TexCoord = vec2(1.0, 1.0);                                                      
    EmitVertex();                                                                   
                                                                                    
    EndPrimitive();                                                                 
}                                                                                   
