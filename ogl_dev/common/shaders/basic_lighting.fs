#version 410 core
                                                                                    
const int MAX_POINT_LIGHTS = 2;                                                     
const int MAX_SPOT_LIGHTS = 2;                                                      

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
uniform sampler2D gColorMap;                                                                
uniform vec3 gEyeWorldPos;                                                                  
uniform float gMatSpecularIntensity;                                                        
uniform float gSpecularPower;                                                               

vec4 CalcLightInternal(BaseLight theLight, vec3 LightDirection, vec3 Normal)
    {
    vec4 AmbientColor = vec4(theLight.Color * theLight.AmbientIntensity, 1.0f);
    float DiffuseFactor = dot(Normal, -LightDirection);
    vec4 DiffuseColor  = vec4(0, 0, 0, 0);                                                  
    vec4 SpecularColor = vec4(0, 0, 0, 0);
    if (DiffuseFactor > 0)
        {
         DiffuseColor = vec4(theLight.Color * theLight.DiffuseIntensity * DiffuseFactor, 1.0f);
  
        vec3 VertexToEye = normalize(gEyeWorldPos - WorldPos0);                             
        vec3 LightReflect = normalize(reflect(LightDirection, Normal));
        float SpecularFactor = dot(VertexToEye, LightReflect);                              
        if (SpecularFactor > 0)
            {
            SpecularFactor = pow(SpecularFactor, gSpecularPower);
            SpecularColor = vec4(theLight.Color * gMatSpecularIntensity * SpecularFactor, 1.0f);
            }
        }
    return (AmbientColor + DiffuseColor + SpecularColor);                                   
}                                                                                           
                                                                                            
vec4 CalcDirectionalLight(vec3 Normal)                                                      
    {
    return CalcLightInternal(gDirectionalLight.Base, gDirectionalLight.Direction, Normal);
    }

vec4 CalcPointLight(PointLight thelight, vec3 Normal)
    {
    vec3 LightDirection = WorldPos0 - thelight.Position;
    float Distance = length(LightDirection);                                                
    LightDirection = normalize(LightDirection);                                             
                                                                                            
    vec4 Color = CalcLightInternal(thelight.Base, LightDirection, Normal);
    float Atten =  thelight.Atten.Constant +
                         thelight.Atten.Linear * Distance +
                         thelight.Atten.Exp * Distance * Distance;
    if (Atten != 0.0)
        Color = Color / Atten;
    return Color;
    }
 
vec4 CalcSpotLight(SpotLight spot, vec3 Normal)
    {
    vec3 LightToPixel = normalize(WorldPos0 - spot.Point.Position);
    float SpotFactor = dot(LightToPixel, spot.Direction);
    vec4 Color = vec4(0, 0, 0, 0);
        SpotFactor = 0.35;
    if (SpotFactor > spot.Cutoff)
        {
        Color = CalcPointLight(spot.Point, Normal);
        Color * (1.0 - (1.0 - SpotFactor) / (1.0 - spot.Cutoff));
        }
    return Color;
    }

void main()                                                                                 
    {
    vec3 Normal = normalize(Normal0);
    
    vec4 TotalLight = CalcDirectionalLight(Normal);
    for (int i = 0 ; i < gNumPointLights; i++)
        {
        TotalLight = TotalLight + CalcPointLight(gPointLights[i], Normal);
        }
    for (int i = 0 ; i < gNumSpotLights; i++)
        {
        TotalLight = TotalLight + CalcSpotLight(gSpotLights[i], Normal);
        }
    FragColor = texture(gColorMap, TexCoord0.xy) * TotalLight;
    }
