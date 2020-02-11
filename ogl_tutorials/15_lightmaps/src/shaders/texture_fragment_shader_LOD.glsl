#version 410 core

// Interpolated values from the vertex shaders
in vec2 UV;

out vec3 color;
uniform sampler2D myTextureSampler;

void main()
    {
	// Output color = color of the texture at the specified UV
    //  -2 is the bias which means that OpenGL will select two mipmaps below
    //  the one it should have taken (so it’s 16 times larger)
	color = texture(myTextureSampler, UV, -2.0).rgb;
    }
