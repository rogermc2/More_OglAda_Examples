#version 410 core

in vec2 UV;
in vec4 ShadowCoord;

layout(location = 0) out vec3 color;

uniform sampler2D myTextureSampler;
uniform sampler2DShadow shadowMap;

void main()
    {
	// Light emission properties
	vec3 LightColor = vec3(1,1,1);
	
	// Material properties
	vec3 MaterialDiffuseColor = texture( myTextureSampler, UV ).rgb;

	float visibility = texture( shadowMap, vec3(ShadowCoord.xy, (ShadowCoord.z)/ShadowCoord.w) );

	color = visibility * MaterialDiffuseColor * LightColor;
    }
