

with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Maths;
with Program_Loader;

Package body Lighting_Technique_26 is

    function Point_Name (Index : GL.Types.Int; Unif : String) return String ;
    function Spot_Name (Index : GL.Types.Int; Unif : String) return String;

    --  -------------------------------------------------------------------------

    function Get_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                   return GL.Uniforms.Uniform is
    begin
        return GL.Objects.Programs.Uniform_Location (Light_Program (theTechnique), Uniform_Name);
    end Get_Uniform_Location;

    --  -------------------------------------------------------------------------

    function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program is
    begin
        return theTechnique.Lighting_Program;
    end Light_Program;

    --  -------------------------------------------------------------------------

    function Init (theTechnique : out Technique) return Boolean is
        use Program_Loader;
        use  GL.Objects.Shaders;
        Name                  : String (1 .. 128);
    begin
        theTechnique.Lighting_Program := Program_From
          ((Src ("src/shaders/lighting.vs", Vertex_Shader),
           Src ("src/shaders/lighting.fs", Fragment_Shader)));

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

        for index in GL.Types.Int range 1 .. Point_Lights_Location_Array'Size loop
            Name := Point_Name (index, "Base.Color");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Color :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Base.AmbientIntensity");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Ambient_Intensity :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Base.DiffuseIntensity");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Diffuse_Intensity :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Position");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Position :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Atten.Constant");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Constant_Atten :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Atten.Linear");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Linear :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Point_Name (index, "Atten.Exp");
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Exp :=
              Get_Uniform_Location (theTechnique, Name);
        end loop;

        for index in GL.Types.Int range 1 .. Spot_Lights_Location_Array'Size loop
            Name := Spot_Name (index, "Base.Base.Color");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Color :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Base.Base.AmbientIntensity");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Ambient_Intensity :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Base.Position");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Position :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Direction");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Direction :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "CutOff");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Cutoff :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Base.DiffuseIntensity");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Diffuse_Intensity :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Atten.Constant");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Constant_Atten :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Atten.Linear");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Linear :=
              Get_Uniform_Location (theTechnique, Name);
            Name := Spot_Name (index, "Atten.Exp");
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Exp :=
              Get_Uniform_Location (theTechnique, Name);
        end loop;
        return True;
    end Init;

    --  -------------------------------------------------------------------------

    function Point_Name (Index : GL.Types.Int; Unif : String) return String is
        use Ada.Strings.Unbounded;
        use GL.Types;
    begin
        return To_String ("gPointLights[" &
                            Trim (To_Unbounded_String (Int'Image (Index - 1)), Ada.Strings.Left)
                          & "]." & Unif);
    end Point_Name;

    --  -------------------------------------------------------------------------

    procedure Set_Colour_Texture_Unit (theTechnique : Technique;
                                       Texture_Unit : GL.Types.Int) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
        GL.Uniforms.Set_Int (theTechnique.Colour_Map_Location, Texture_Unit);
    end Set_Colour_Texture_Unit;

    --  -------------------------------------------------------------------------

    procedure Set_Directional_Light (theTechnique : Technique; Light : Direct_Light) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Location.Color, Light.Base.Colour);
        GL.Uniforms.Set_Single
          (theTechnique.Direct_Light_Location.Ambient_Intensity, Light.Base.Ambient_Intensity);
        GL.Uniforms.Set_Single
          (theTechnique.Direct_Light_Location.Diffuse_Intensity, Light.Base.Diffuse_Intensity);
        GL.Uniforms.Set_Single
          (theTechnique.Direct_Light_Location.Direction, Maths.Normalized (Light.Direction));
    end Set_Directional_Light;

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

   procedure Set_WVP (theTechnique : Technique;
                      WVP : GL.Types.Singles.Matrix4) is
    begin
        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
        GL.Uniforms.Set_Single (theTechnique.WVP_Location, WVP);
    end Set_WVP;

    --  -------------------------------------------------------------------------

    function Spot_Name (Index : GL.Types.Int; Unif : String) return String is
        use Ada.Strings.Unbounded;
        use GL.Types;
    begin
        return To_String ("gSpotLights[" &
                            Trim (To_Unbounded_String (Int'Image (Index - 1)), Ada.Strings.Left)
                          & "]." & Unif);
    end Spot_Name;

    --  -------------------------------------------------------------------------

    procedure Use_Program (theTechnique : Technique) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders.Lists;
    begin
        if GL.Objects.Programs.Validate_Status (theTechnique.Lighting_Program) then
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
