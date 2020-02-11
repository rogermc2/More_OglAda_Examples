#version 410 core

in vec2 UV;
in vec4 ShadowCoord;

out vec3 color;

uniform sampler2D myTextureSampler;
uniform sampler2DShadow shadowMap;

void main()
    {
    float bias = 0.2;
    float visibility = 1.0;
	vec3 LightColor = vec3(1,1,1);
	vec3 MaterialDiffuseColor = texture(myTextureSampler, UV).rgb;
    //  texture(shadowMap, ShadowCoord.xy).z is the distance between the light
    //  and the nearest occluder.
    //  ShadowCoord.z is the distance between the light and the current fragment.
//	float visibility = texture(shadowMap, vec3(ShadowCoord.xy, (ShadowCoord.z)/ShadowCoord.w));
    if (texture(shadowMap, vec3(ShadowCoord.xy, (ShadowCoord.z)/ShadowCoord.w)) < ShadowCoord.z - bias)
        {
        visibility = 0.5;
        }
	color = visibility * MaterialDiffuseColor * LightColor;
    }
