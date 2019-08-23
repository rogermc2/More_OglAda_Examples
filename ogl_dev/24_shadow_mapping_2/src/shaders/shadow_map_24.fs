#version 410 core

// The fragment shader is only used by the screen rendering pass
// The shadow map texture was created with the type GL_DEPTH_COMPONENT as its internal format.
// Therefore the basic texel is a single floating point value and not a color.
// This is why '.x' is used during sampling.
// The result will be some variation of grey (white at the far clipping plane and black at the near clipping plane).

in vec2 TexCoordOut;                                                                
uniform sampler2D gShadowMap;                                                       
                                                                                    
out vec4 FragColor;                                                                 
                                                                                    
void main()                                                                         
{                                                                                   
    float Depth = texture(gShadowMap, TexCoordOut).x;
    // after sampling the depth from the shadow map sharpen it by scaling the distance of the
    // current point to the far edge (where Z is 1.0)
    // then subtract the result from 1.0 again.
    Depth = 1.0 - (1.0 - Depth) * 25.0;   //   25 Depth - 24
    FragColor = vec4(Depth);                                                        
}
