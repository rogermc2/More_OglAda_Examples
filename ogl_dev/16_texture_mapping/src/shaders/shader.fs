#version 410 core

in vec2 TexCoord0;

out vec4 FragColor;

uniform sampler2D gSampler;  //  tells the shader which texture unit to use.

void main()
{
    FragColor = texture(gSampler, TexCoord0.xy);  //  Sampled filtered texel
}
