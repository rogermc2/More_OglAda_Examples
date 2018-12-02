#version 410 core
                                                                                    
in vec2 TexCoordOut;
uniform sampler2D gShadowMap;
                                                                                    
out vec4 fragment_colour;
                                                                                    
void main()                                                                         
    {                                                                                   
    fragment_colour = texture(gShadowMap, TexCoordOut);
    }
