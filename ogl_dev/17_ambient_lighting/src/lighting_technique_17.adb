
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Objects.Shaders;
with GL.Uniforms;

with Maths;
with Program_Loader;

package body Lighting_Technique_17 is 

    type Light_Location is record
      Colour            : GL.Uniforms.Uniform;
      Ambient_Intensity : GL.Uniforms.Uniform;
    end record;
     
    WVP_Location               : GL.Uniforms.Uniform;
    Sampler_Location           : GL.Uniforms.Uniform;
    Directional_Light_Location : Light_Location;

    --  -------------------------------------------------------------------------
 
   function Get_Directional_Ambient (Light : Directional_Light) return Single is
    begin
        return Light.Ambient_Intensity;
    end Get_Directional_Ambient;

    --  -------------------------------------------------------------------------

  function Init (Shader_Program : in out GL.Objects.Programs.Program) return Boolean is
      use GL.Objects.Shaders;
      use Program_Loader;
      OK  : Boolean := False;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders/ambient_lighting.vs", Vertex_Shader),
         Src ("src/shaders/ambient_lighting.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (Shader_Program);
      if not OK then
         Put_Line ("Build_Shader_Program, Shader_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
      else
         GL.Objects.Programs.Use_Program (Shader_Program);
         WVP_Location :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "gWVP");
         Sampler_Location :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "gSampler");
          Directional_Light_Location.Colour :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "gDirectionalLight.Color");
          Directional_Light_Location.Ambient_Intensity :=
           GL.Objects.Programs.Uniform_Location (Shader_Program, "gDirectionalLight.AmbientIntensity");
      end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Lighting_Technique_17.Init.");
         raise;
   end Init;

    --  -------------------------------------------------------------------------

    procedure Set_WVP (WVP : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (WVP_Location, WVP);    
    end Set_WVP;
 
    --  -------------------------------------------------------------------------
   
    procedure Set_Texture_Unit (Texture_Unit : Int) is
    begin
        GL.Uniforms.Set_Int (Sampler_Location, Texture_Unit);
    end Set_Texture_Unit;

    --  -------------------------------------------------------------------------

    procedure Set_Directional_Ambient (Light : in out Directional_Light;
                                       Ambient : Single) is
    begin
        Light.Ambient_Intensity := Ambient;
    end Set_Directional_Ambient;

    --  -------------------------------------------------------------------------
    
    procedure Set_Directional_Light_Locations (Light : Directional_Light) is
    begin
      GL.Uniforms.Set_Single (Directional_Light_Location.Colour, Light.Colour);
      GL.Uniforms.Set_Single (Directional_Light_Location.Ambient_Intensity,
                              Light.Ambient_Intensity);
    end Set_Directional_Light_Locations;

    --  -------------------------------------------------------------------------

end Lighting_Technique_17;
