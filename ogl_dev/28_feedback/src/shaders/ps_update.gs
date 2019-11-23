#version 410 core
                                                                                    
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
uniform float     gSecondaryShellLifetime;
uniform float     gShellLifetime;

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
    float Age = Age0[0] + gDeltaTimeMillis;
    float TexCoord1;
    vec3  Dir;
    float DeltaTimeSecs = gDeltaTimeMillis / 1000.0;  //  1000.0f
    vec3  DeltaP;
    vec3  DeltaV;

    if (Type0[0] == PARTICLE_TYPE_LAUNCHER)
        {
        if (Age >= gLauncherLifetime)
            {
            Type1 = PARTICLE_TYPE_SHELL;
            Position1 = Position0[0];     //  Shell starts at launcher
            TexCoord1 = gTime / 1000.0;
            Dir = GetRandomDir(TexCoord1);
            Dir.y = max(Dir.y, 0.5);
            Velocity1 = normalize(Dir) / 20.0;  //  20.0
            Age1 = 0.0;
            EmitVertex();
            EndPrimitive();
            Age = 0.0;
            }  //  end if Age >= gLauncherLifetime
   
        Type1 = PARTICLE_TYPE_LAUNCHER;
        Position1 = Position0[0];  //  Launcher position shouldn't change
        Velocity1 = Velocity0[0];
        Age1 = Age;
        EmitVertex();
        EndPrimitive();
        }  // end PARTICLE_TYPE_LAUNCHER

    else  //  Not PARTICLE_TYPE_LAUNCHER
        {
        if (Type0[0] == PARTICLE_TYPE_SHELL)
            {
            if (Age < gShellLifetime)
                {  //  Update shell position and velocity
                Type1 = PARTICLE_TYPE_SHELL;
                DeltaP = DeltaTimeSecs * Velocity0[0];
                DeltaV = vec3(DeltaTimeSecs) * (0.0, -9.81, 0.0);
                Position1 = Position0[0] + DeltaP;
                Velocity1 = Velocity0[0] + DeltaV;
                Age1 = Age;
                EmitVertex();
                EndPrimitive();
                }
 
            else  //  Age >= gShellLifetime
                {
                //  Launch 10 secondary shells from current shell position
                for (int i = 0 ; i < 10 ; i++)
                    {
                    Type1 = PARTICLE_TYPE_SECONDARY_SHELL;
                    Position1 = Position0[0];
                    // TexCoord1 = (gTime + i) / 1000.0;
                    TexCoord1 = fract (gTime + i / 10.0);
                    Dir = GetRandomDir(TexCoord1);
                    Velocity1 = normalize(Dir) / 20.0;
                    Age1 = 0.0f;
                    EmitVertex();
                    EndPrimitive();
                    }
                }  //  end Age >= gShellLifetime
            }  //  end if PARTICLE_TYPE_SHELL

        else   //  Not PARTICLE_TYPE_SHELL therefore PARTICLE_TYPE_SECONDARY_SHELL
            {
            if (Age < gSecondaryShellLifetime)
                {  //  Update secondary shell position and velocity
                Type1 = PARTICLE_TYPE_SECONDARY_SHELL;
                DeltaP = DeltaTimeSecs * Velocity0[0];
                    DeltaV = vec3(DeltaTimeSecs) * (0.0, -9.81, 0.0);
                Position1 = Position0[0] + DeltaP;
                Velocity1 = Velocity0[0] + DeltaV;
                Age1 = Age;
                EmitVertex();
                EndPrimitive();
                }  //  end Age < gSecondaryShellLifetime
            }  //  end PARTICLE_TYPE_SECONDARY_SHELL
        }   //  end Not PARTICLE_TYPE_LAUNCHER
    }   // end main
