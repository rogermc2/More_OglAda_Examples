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
                                                                                            
float CalcShadowFactor(vec4 LightPos)
    {
    //  After multiplying the vertex position by the projection matrix the coordinates are
    //  in Clip Space and after performing a perspective divide the coordinates are in
    //  NDC Space (Normalized Device Coordinates).
    vec3 ProjCoords = LightPos.xyz / LightPos.w;
    vec2 UVCoords;                                                                          
    UVCoords.x = 0.5 * ProjCoords.x + 0.5;                                                  
    UVCoords.y = 0.5 * ProjCoords.y + 0.5;
    if (UVCoords.x < 0.0)
        UVCoords.x = 0.0;
    else if (UVCoords.x > 1.0)
        UVCoords.x = 1.0;
    if (UVCoords.y < 0.0)
        UVCoords.y = 0.0;
    else if (UVCoords.y > 1.0)
        UVCoords.y = 1.0;
        
    float z = 0.5 * ProjCoords.z + 0.5;
    float Depth = texture(gShadowMap, UVCoords).x;
//     Depth = texture(gShadowMap, TexCoord0).x;
    if (Depth < z + 0.00001)                                                                 
        return 0.1;
    else                                                                                    
        return 1.0;
    }
                                                                                            
vec4 CalcLightInternal(BaseLight Light, vec3 LightDirection, vec3 Normal,            
                       float ShadowFactor)                                                  
    {
    vec4 AmbientColor = vec4(Light.Color * Light.AmbientIntensity, 1.0f);
    float DiffuseFactor = dot(Normal, -LightDirection);
                                                                                            
    vec4 DiffuseColor  = vec4(0.0, 0, 0, 0);
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
                                                                                            
vec4 CalcPointLight(PointLight point, vec3 Normal, vec4 LightPos)
    {
    vec3 LightDirection = WorldPos0 - point.Position;
    float Distance = length(LightDirection);                                                
    LightDirection = normalize(LightDirection);                                             
    float ShadowFactor = CalcShadowFactor(LightPos);
    vec4 Color = CalcLightInternal(point.Base, LightDirection, Normal, ShadowFactor);
    float Att =  point.Atten.Constant +
                 point.Atten.Linear * Distance +
                 point.Atten.Exp * Distance * Distance;
    if (Att > 0.0)
        {
        Color = Color / Att;
        }
    return Color;
    }
                                                                                            
vec4 CalcSpotLight(SpotLight spot, vec3 Normal, vec4 LightPos)
    {
    vec4 Color = vec4(0, 0, 0, 0);
    vec3 LightToPixel = normalize(WorldPos0 - spot.Point.Position);
    float SpotFactor = dot(LightToPixel, spot.Direction);
    if (SpotFactor > spot.Cutoff)
        {
        Color = CalcPointLight(spot.Point, Normal, LightPos);
        Color = Color * (1.0 - (1.0 - SpotFactor) / (1.0 - spot.Cutoff));
        }
    return Color;
    }
                                                                                            
void main()                                                                                 
    {
    vec3 Normal = normalize(Normal0);
    vec4 TotalLight = CalcDirectionalLight(Normal);
    vec4 SampledColor = texture(gSampler, TexCoord0.xy);
        
    for (int i = 0 ; i < gNumPointLights ; i++)
        {
        TotalLight = TotalLight + CalcPointLight(gPointLights[i], Normal, LightSpacePos);
        }
    for (int i = 0 ; i < gNumSpotLights ; i++)
        {
        TotalLight = TotalLight + CalcSpotLight(gSpotLights[i], Normal, LightSpacePos);
        }
    FragColor = SampledColor * TotalLight;
    }
