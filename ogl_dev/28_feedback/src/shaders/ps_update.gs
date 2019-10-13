#version 410 core
//  #extension GL_EXT_gpu_shader4 : enable
//  #extension GL_EXT_geometry_shader4: enable
                                                                                    
layout(points) in;                                                                  
layout(points) out;                                                                 
layout(max_vertices = 30) out;                                                      
                                                                                    
in float Type0[];                                                                   
in vec3  Position0[];
in vec3  Velocity0[];
in float Age0[];
                                                                                    
out float Type1;                                                                    
out vec3  Position1;
out vec3  Velocity1;
out float Age1;                                                                     
                                                                                    
uniform float     gDeltaTimeMillis;
uniform float     gTime;
uniform sampler1D gRandomTexture;                                                   
uniform float     gLauncherLifetime;
uniform float     gShellLifetime;
uniform float     gSecondaryShellLifetime;
                                                                                    
#define PARTICLE_TYPE_LAUNCHER        0.0f
#define PARTICLE_TYPE_SHELL           1.0f
#define PARTICLE_TYPE_SECONDARY_SHELL 2.0f                                          
                                                                                    
vec3 GetRandomDir(float TexCoord)                                                   
    {                                                                                   
     vec3 Dir = texture(gRandomTexture, TexCoord).xyz;                              
     Dir = Dir - vec3(0.5, 0.5, 0.5);                                                    
     return Dir;                                                                    
    }                                                                                   
                                                                                    
void main()                                                                         
    {
    vec3 Dir;
    float Age = Age0[0] + gDeltaTimeMillis;
    float DeltaTimeSecs = gDeltaTimeMillis / 1000.0f;
    float t1;
    float t2;
    vec3 DeltaP;
    vec3 DeltaV;
                                                                                    
    if (Type0[0] == PARTICLE_TYPE_LAUNCHER)
        {
        if (Age >= gLauncherLifetime)
            {                                             
            Type1 = PARTICLE_TYPE_SHELL;                                            
            Position1 = Position0[0]+vec3(1.0,-1.0,1.0);
            Dir = GetRandomDir(gTime/1000.0);
            Dir.y = max(Dir.y, 0.5);
            //    Velocity1 = normalize(Dir) / 20.0;
            Velocity1 = normalize(Dir) / 2.0;
            Age1 = 0.0;                                                             
            EmitVertex();
            EndPrimitive();
            Age = 0.0;                                                              
            }
                                                                                    
        Type1 = PARTICLE_TYPE_LAUNCHER;
        Position1 = Position0[0];                                                   
        Velocity1 = Velocity0[0];                                                   
        Age1 = Age;                                                                 
        EmitVertex();                                                               
        EndPrimitive();                                                             
        }
    else
        {
         DeltaTimeSecs = gDeltaTimeMillis / 1000.0f;
         t1 = Age0[0] / 1000.0;
         t2 = Age / 1000.0;
         DeltaP = DeltaTimeSecs * Velocity0[0];
         DeltaV = vec3(DeltaTimeSecs) * (0.0, -9.81, 0.0);
                                                                                    
        if (Type0[0] == PARTICLE_TYPE_SHELL)
            {
	        if (Age < gShellLifetime)
                {
	            Type1 = PARTICLE_TYPE_SHELL;                                        
	            Position1 = Position0[0] + DeltaP;                                  
	            Velocity1 = Velocity0[0] + DeltaV;                                  
	            Age1 = Age;                                                         
	            EmitVertex();
	            EndPrimitive();
	            }
            else  //  Age > gShellLifetime
                {
                for (int i = 0 ; i < 10 ; i++)
                    {
                     Type1 = PARTICLE_TYPE_SECONDARY_SHELL;                         
                     Position1 = Position0[0];                                      
                     Dir = GetRandomDir((gTime + i)/1000.0);
                     Velocity1 = normalize(Dir) / 20.0;                             
                     Age1 = 0.0f;                                                   
                     EmitVertex();
                     EndPrimitive();
                    }
                }
            }  //  end if (Age < gShellLifetime)
        else  //  Type0[0] NE PARTICLE_TYPE_SHELL
            {
            if (Age < gSecondaryShellLifetime)
                {
                Type1 = PARTICLE_TYPE_SECONDARY_SHELL;                              
                Position1 = Position0[0] + DeltaP;                                  
                Velocity1 = Velocity0[0] + DeltaV;                                  
                Age1 = Age;                                                         
                EmitVertex();
                EndPrimitive();
                }
            }
        }
    }
