#version 410 core
                                                                                                                                                                                                                                                        
uniform uint gDrawIndex;                                                            
uniform uint gObjectIndex;                                                          
                                                                                    
out vec3 FragColor;

void main()                                                                         
    {                                                                                   
   FragColor = vec3(float(gObjectIndex), float(gDrawIndex),float(gl_PrimitiveID + 1));                 
    }
