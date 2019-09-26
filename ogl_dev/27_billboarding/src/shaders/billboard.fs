#version 410 core
                                                                                    
uniform sampler2D gColorMap;
varying vec2 TexCoord;
                                                                                    
void main()                                                                         
    {
    gl_FragColor = texture2D(gColorMap, TexCoord);

    if (gl_FragColor.r == 0 && gl_FragColor.g == 0 && gl_FragColor.b == 0)
        {
        discard;                                                                    
        }
    }
