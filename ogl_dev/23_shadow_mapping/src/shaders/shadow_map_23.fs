#version 410 core
                                                                                    
in vec2 TexCoordOut;
uniform sampler2D gShadowMap;
                                                                                    
out vec4 fragment_colour;
                                                                                    
void main()                                                                         
{                                                                                   
    float Depth = texture(gShadowMap, TexCoordOut).x;
    Depth = 1.0 - (1.0 - Depth) * 25.0;
    //  One floating-point number in the constructor sets all components to the same value.
    // This should result in grey
  fragment_colour = vec4(Depth);
 //  fragment_colour = texture(gShadowMap, TexCoordOut.xy);
    //fragment_colour = vec4(1.0,0.0,0.0,1.0);
}
