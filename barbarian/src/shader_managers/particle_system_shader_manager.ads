
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Particle_System_Shader_Manager is

    type Basic_Particle_Uniforms is record
        Degrees_ID           : GL.Uniforms.Uniform := 0;
        Final_Colour_ID      : GL.Uniforms.Uniform := 0;
        Final_Scale_ID       : GL.Uniforms.Uniform := 0;
        Initial_Colour_ID    : GL.Uniforms.Uniform := 0;
        Initial_Scale_ID     : GL.Uniforms.Uniform := 0;
        Lifetime_ID          : GL.Uniforms.Uniform := 0;
        Perspective_View_ID  : GL.Uniforms.Uniform := 0;
        Pixel_Width_ID       : GL.Uniforms.Uniform := 0;
        Texture_Map_ID       : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Degrees (Degrees : Single);
    procedure Set_Final_Colour (Colour : Singles.Vector4);
    procedure Set_Final_Scale (Scale : Single);
    procedure Set_Initial_Colour (Colour : Singles.Vector4);
    procedure Set_Initial_Scale (Scale : Single);
    procedure Set_Lifetime (Lifetime : Single);
    procedure Set_Perspective_View (PV : Singles.Matrix4);
    procedure Set_Pixel_Width (Width : Single);
    procedure Set_Texture_Map (Map : Int);

end Particle_System_Shader_Manager;
