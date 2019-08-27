#version 410 core
                                                                                    
const int MAX_POINT_LIGHTS = 2;                                                     
const int MAX_SPOT_LIGHTS = 2;                                                      
                                                                                    
in vec4 LightSpacePos;                                                              
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
                                                                                            
struct SpotLight                                                                            
{                                                                                           
    PointLight Point;
    vec3 Direction;                                                                         
    float Cutoff;                                                                           
};                                                                                          
                                                                                            
uniform int gNumPointLights;                                                                
uniform int gNumSpotLights;                                                                 
uniform DirectionalLight gDirectionalLight;                                                 
uniform PointLight gPointLights[MAX_POINT_LIGHTS];                                          
uniform SpotLight gSpotLights[MAX_SPOT_LIGHTS];                                             
uniform sampler2D gSampler;                                                                 
uniform sampler2D gShadowMap;                                                               
uniform vec3 gEyeWorldPos;                                                                  
uniform float gMatSpecularIntensity;                                                        
uniform float gSpecularPower;                                                               
                                                                                            
float CalcShadowFactor(vec4 LightSpacePos)                                                  
    {                                                                                           
    vec3 ProjCoords = LightSpacePos.xyz / LightSpacePos.w;                                  
    vec2 UVCoords;                                                                          
    UVCoords.x = 0.5 * ProjCoords.x + 0.5;                                                  
    UVCoords.y = 0.5 * ProjCoords.y + 0.5;                                                  
    float z = 0.5 * ProjCoords.z + 0.5;                                                     
    float Depth = texture(gShadowMap, UVCoords).x;                                          
    if (Depth < z + 0.00001)                                                                 
        return 0.5;                                                                         
    else                                                                                    
        return 1.0;                                                                         
    }                                                                                           
                                                                                            
vec4 CalcLightInternal(BaseLight Light, vec3 LightDirection, vec3 Normal,            
                       float ShadowFactor)                                                  
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
            SpecularColor = vec4(Light.Color, 1.0f) * gMatSpecularIntensity * SpecularFactor;
            }
        }                                                                    
    return (AmbientColor + ShadowFactor * (DiffuseColor + SpecularColor));
    }
                                                                                            
vec4 CalcDirectionalLight(vec3 Normal)                                                      
    {
    return CalcLightInternal(gDirectionalLight.Base, gDirectionalLight.Direction, Normal, 1.0);  
    }
                                                                                            
vec4 CalcPointLight(PointLight light, vec3 Normal, vec4 LightSpacePos)
    {
    vec3 LightDirection = WorldPos0 - light.Position;
    float Distance = length(LightDirection);                                                
    LightDirection = normalize(LightDirection);                                             
    float ShadowFactor = CalcShadowFactor(LightSpacePos);
                                                                                            
    vec4 Color = CalcLightInternal(light.Base, LightDirection, Normal, ShadowFactor);
    float Att =  light.Atten.Constant +
                 light.Atten.Linear * Distance +
                 light.Atten.Exp * Distance * Distance;
    return Color / Att;
    }
                                                                                            
vec4 CalcSpotLight(SpotLight l, vec3 Normal, vec4 LightSpacePos)                     
    {
    vec3 LightToPixel = normalize(WorldPos0 - l.Point.Position);
    float SpotFactor = dot(LightToPixel, l.Direction);
    vec4 Color = vec4(0,0,0,0);
                                                                                            
    if (SpotFactor > l.Cutoff)
        {
        Color = CalcPointLight(l.Point, Normal, LightSpacePos);
        Color = Color * (1.0 - (1.0 - SpotFactor) / (1.0 - l.Cutoff));
        }
    return Color;
    }
                                                                                            
void main()                                                                                 
    {
    vec3 Normal = normalize(Normal0);                                                       
    vec4 TotalLight = CalcDirectionalLight(Normal);                                         
                                                                                            
    for (int i = 0 ; i < gNumPointLights ; i++)
        {
        TotalLight = TotalLight + CalcPointLight(gPointLights[i], Normal, LightSpacePos);
        }
    for (int i = 0 ; i < gNumSpotLights ; i++)
        {
        TotalLight = TotalLight + CalcSpotLight(gSpotLights[i], Normal, LightSpacePos);
        }
                                                                                            
    vec4 SampledColor = texture(gSampler, TexCoord0.xy);                                  
    FragColor = SampledColor * TotalLight;                                                  
}
