#version 120
                                                                                    
attribute vec3 Position;
attribute vec2 TexCoord;
attribute vec3 Normal;
attribute vec3 Tangent;
                                                                                    
uniform mat4 gWVP;                                                                  
uniform mat4 gLightWVP;                                                             
uniform mat4 gWorld;                                                                
                                                                                    
varying vec4 LightSpacePos;
varying vec2 TexCoord0;
varying vec3 Normal0;
varying vec3 WorldPos0;
varying vec3 Tangent0;                                                                  
                                                                                    
void main()                                                                         
{                                                                                   
    gl_Position   = gWVP * vec4(Position, 1.0);                                     
    LightSpacePos = gLightWVP * vec4(Position, 1.0);                                
    TexCoord0     = TexCoord;                                                       
    Normal0       = (gWorld * vec4(Normal, 0.0)).xyz;                               
    Tangent0      = (gWorld * vec4(Tangent, 0.0)).xyz;                              
    WorldPos0     = (gWorld * vec4(Position, 1.0)).xyz;                             
}
