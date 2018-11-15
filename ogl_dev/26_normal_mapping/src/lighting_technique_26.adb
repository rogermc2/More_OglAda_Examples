

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

Package body Lighting_Technique_26 is

   function Light_Program (theTechnique : Technique)
                               return GL.Objects.Programs.Program is
   begin
      return theTechnique.Lighting_Program;
   end Light_Program;

     --  -------------------------------------------------------------------------

   function Init (theTechnique : out Technique) return Boolean is
      use Program_Loader;
      use  GL.Objects.Shaders;
   begin
      theTechnique.Lighting_Program := Program_From
        ((Src ("src/shaders/billboard.vs", Vertex_Shader),
          Src ("src/shaders/billboard.fs", Fragment_Shader),
          Src ("src/shaders/billboard.gs", Geometry_Shader)));

      GL.Objects.Programs.Use_Program  (theTechnique.Lighting_Program);
      theTechnique.WVP_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gWVP");
      theTechnique.Light_WVP_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gLightWVP");
      theTechnique.World_Matrix_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gWorld");
      theTechnique.Colour_Map_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gColorMap");
      theTechnique.Shadow_Map_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gShadowMap");
      theTechnique.Normal_Map_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gNormalMap");
      theTechnique.Eye_World_Pos_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gEyeWorldPos");
      theTechnique.Direct_Light_Location.Color := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gDirectionalLight.Base.Color");
      theTechnique.Direct_Light_Location.Ambient_Intensity := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gDirectionalLight.Base.AmbientIntensity");
      theTechnique.Direct_Light_Location.Diffuse_Intensity := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gDirectionalLight.Base.DiffuseIntensity");
      theTechnique.Direct_Light_Location.Direction := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gDirectionalLight.Base.Direction");
      theTechnique.Mat_Specular_Intensity_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gMatSpecularIntensity");
      theTechnique.Mat_Specular_Power_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gSpecularPower");
      theTechnique.Num_Point_Lights_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gNumPointLights");
      theTechnique.Num_Spot_Lights_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Lighting_Program, "gNumSpotLights");
      return True;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Colour_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Int (theTechnique.Colour_Map_Location, Texture_Unit);
   end Set_Colour_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Eye_World_Position (theTechnique : Technique;
                                     Position : GL.Types.Singles.Vector3) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Single (theTechnique.Eye_World_Pos_Location, Position);
   end Set_Eye_World_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Light_WVP_Position (theTechnique : Technique;
                                     Position : GL.Types.Singles.Vector3) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Single (theTechnique.WVP_Location, Position);
   end Set_Light_WVP_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Normal_Map_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Int (theTechnique.Normal_Map_Location, Texture_Unit);
   end Set_Normal_Map_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Shadow_Map_Texture_Unit (theTechnique : Technique;
                                          Texture_Unit : GL.Types.Int) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Int (theTechnique.Shadow_Map_Location, Texture_Unit);
   end Set_Shadow_Map_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_World_Matrix (theTechnique : Technique;
                               WVP : GL.Types.Singles.Matrix4) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      GL.Uniforms.Set_Single (theTechnique.World_Matrix_Location, WVP);
   end Set_World_Matrix;

   --  -------------------------------------------------------------------------

     procedure Use_Program (theTechnique : Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
   begin
              if not GL.Objects.Programs.Validate_Status (theTechnique.Lighting_Program) then
      --              Put_Line ("Billboard_Technique.Use_Program Update_Program validation failed.");
      --          else
      --              Put_Line ("Billboard_Technique.Use_Program Update_Program validated.");
      declare
         Shaders_List : GL.Objects.Shaders.Lists.List :=
                          GL.Objects.Programs.Attached_Shaders (theTechnique.Lighting_Program);
         Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
      begin
         if Curs = GL.Objects.Shaders.Lists.No_Element then
            Put_Line ("Lighting_Technique_26.Use_Program, Shaders list is empty");
         else
            GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
         end if;
      end;  -- declare block
              end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Lighting_Technique_26.Use_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------

end Lighting_Technique_26;
