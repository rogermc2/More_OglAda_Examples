#version 410 core

// Interpolated values from the vertex shaders
in vec2 UV;
in vec4 particlecolor;

// Ouput data
out vec4 colour;

uniform sampler2D myTextureSampler;

void main(){
	// Output color = color of the texture at the specified UV
	colour = texture( myTextureSampler, UV ) * particlecolor;

}
