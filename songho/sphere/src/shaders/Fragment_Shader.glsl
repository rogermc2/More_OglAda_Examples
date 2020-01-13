#version 410 core

// Interpolated values from the vertex shaders
in vec2 texCoord0;
in vec3 esVertex;
in vec3 esNormal;

out vec4 fragColor;

uniform vec4 lightPosition;             // should be in the eye space
uniform vec4 lightAmbient;              // light ambient color
uniform vec4 lightDiffuse;              // light diffuse color
uniform vec4 lightSpecular;             // light specular color
uniform vec4 materialAmbient;           // material ambient color
uniform vec4 materialDiffuse;           // material diffuse color
uniform vec4 materialSpecular;          // material specular color
uniform float materialShininess;        // material specular shininess
uniform sampler2D map0;                 // texture map #1
uniform bool textureUsed;               // flag for texture

void main()
    {
    vec3 normal = normalize(esNormal);
    vec3 light;
    if(lightPosition.w == 0.0)
        {
        light = normalize(lightPosition.xyz);
        }
    else
        {
        light = normalize(lightPosition.xyz - esVertex);
        }
    vec3 view = normalize(-esVertex);
    vec3 halfv = normalize(light + view);
    // begin with ambient
    vec3 color = lightAmbient.rgb * materialAmbient.rgb;

    float dotNL = max(dot(normal, light), 0.0);
            // add diffuse
    color = color + lightDiffuse.rgb * materialDiffuse.rgb * dotNL;

    // modulate texture map
    if(textureUsed)
        color = color * texture(map0, texCoord0).rgb;
    float dotNH = max(dot(normal, halfv), 0.0);
    // add specular
    color = color + pow(dotNH, materialShininess) * lightSpecular.rgb * materialSpecular.rgb;
    fragColor = 3 * vec4(color, materialDiffuse.a);
   // fragColor =  vec4(texCoord0.x,0,0, 0.0);
    }
