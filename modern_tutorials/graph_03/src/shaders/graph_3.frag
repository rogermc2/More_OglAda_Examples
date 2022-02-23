#version 410 core

uniform vec4 colour;

out vec4 FragColour;

void main(void)
    {
	FragColour = colour;
    }
