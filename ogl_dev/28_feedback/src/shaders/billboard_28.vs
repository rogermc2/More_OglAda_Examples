#version 410 core
                                                                                    
layout (location = 0) in vec3 Position;                                             
                                                                                    
void main()                                                                         
{
    gl_PointSize = 20.0;
    gl_Position = vec4(Position, 1.0);                                              
}                                                                                   
