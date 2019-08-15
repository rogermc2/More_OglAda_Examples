#version 410 core
                                                                                    
const int MAX_POINT_LIGHTS = 2;
                       
in vec2 TexCoord0;
in vec3 Normal0;
in vec3 WorldPos0;
                                                                                    
out vec4 FragColor;
                                                                                    
struct BaseLight                                                                    
    {
    vec3 Color;                                                                     
    float AmbientIntensity;                                                         
    float DiffuseIntensity;                                                         
    };
                                                                                    
struct DirectionalLight                                                             
    {
    BaseLight Base;                                                                 
    vec3 Direction;                                                                 
    };
                                                                                    
struct Attenuation                                                                  
    {
    float Constant;                                                                 
    float Linear;                                                                   
    float Exp;                                                                      
    };
                                                                                    
struct PointLight                                                                           
    {
    BaseLight Base;                                                                         
    vec3 Position;                                                                          
    Attenuation Atten;                                                                      
    };

uniform int gNumPointLights;
uniform DirectionalLight gDirectionalLight;
uniform PointLight gPointLights[MAX_POINT_LIGHTS];
uniform sampler2D gSampler;
uniform vec3 gEyeWorldPos;
uniform float gMatSpecularIntensity;
uniform float gSpecularPower;

vec4 CalcLightInternal(BaseLight Light, vec3 LightDirection, vec3 Normal)
    {
    vec4 AmbientColor = vec4(Light.Color * Light.AmbientIntensity, 1.0f);
    float DiffuseFactor = dot(Normal, -LightDirection);
    vec4 DiffuseColor  = vec4(0, 0, 0, 0);
    vec4 SpecularColor = vec4(0, 0, 0, 0);
        
    if (DiffuseFactor > 0)
        {
        DiffuseColor = vec4(Light.Color * Light.DiffuseIntensity * DiffuseFactor, 1.0f);
        vec3 VertexToEye = normalize(gEyeWorldPos - WorldPos0);
        vec3 LightReflect = normalize(reflect(LightDirection, Normal));
        float SpecularFactor = dot(VertexToEye, LightReflect);
            
        if (SpecularFactor > 0)
            {
            SpecularFactor = pow(SpecularFactor, gSpecularPower);
            SpecularColor = vec4(Light.Color * gMatSpecularIntensity * SpecularFactor, 1.0f);
            }
        }
    return (AmbientColor + DiffuseColor + SpecularColor);                  
}

vec4 CalcDirectionalLight(vec3 Normal)
    {
    return CalcLightInternal(gDirectionalLight.Base, gDirectionalLight.Direction, Normal);  
    }

vec4 CalcPointLight(PointLight l, vec3 Normal)
{
    vec3 LightDirection = WorldPos0 - l.Position;
    float Distance = length(LightDirection);
    LightDirection = normalize(LightDirection);
    
    vec4 Color = CalcLightInternal(l.Base, LightDirection, Normal);
    float Att =  l.Atten.Constant +
    l.Atten.Linear * Distance +
    l.Atten.Exp * Distance * Distance;
    Color = Color / Att;
    return Color;
}

void main()
    {
    vec3 Normal = normalize(Normal0);
    vec4 TotalLight = CalcDirectionalLight(Normal);
        
    for (int i = 0 ; i < gNumPointLights ; i++)
        {
        TotalLight = TotalLight + CalcPointLight(gPointLights[i], Normal);
        }
         
    FragColor = texture (gSampler, TexCoord0.xy) * TotalLight;
}
