

with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

Package body Lighting_Technique_26 is

    function Point_Name (Index : GL.Types.Int; Unif : String) return String;
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
    begin
        theTechnique.Lighting_Program := Program_From
          ((Src ("src/shaders/lighting_26.vs", Vertex_Shader),
           Src ("src/shaders/lighting_26.fs", Fragment_Shader)));

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
          (theTechnique.Lighting_Program, "gDirectionalLight.Direction");
        theTechnique.Mat_Specular_Intensity_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gMatSpecularIntensity");
        theTechnique.Mat_Specular_Power_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gSpecularPower");
        theTechnique.Num_Point_Lights_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gNumPointLights");
        theTechnique.Num_Spot_Lights_Location := GL.Objects.Programs.Uniform_Location
          (theTechnique.Lighting_Program, "gNumSpotLights");

        for index in GL.Types.Int range 1 .. Point_Lights_Location_Array'Length loop
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Color :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Base.Color"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Ambient_Intensity :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Base.AmbientIntensity"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Diffuse_Intensity :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Base.DiffuseIntensity"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Position :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Position"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Constant_Atten :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Atten.Constant"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Linear :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Atten.Linear"));
            theTechnique.Point_Lights_Locations (GL.Types.UInt (index)).Atten.Exp :=
              Get_Uniform_Location (theTechnique, Point_Name (index, "Atten.Exp"));
        end loop;

        for index in GL.Types.Int range 1 .. Spot_Lights_Location_Array'Length loop
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Color :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Base.Base.Color"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Ambient_Intensity :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Base.Base.AmbientIntensity"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Position :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Base.Position"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Direction :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Direction"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Cutoff :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "CutOff"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Diffuse_Intensity :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Base.DiffuseIntensity"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Constant_Atten :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Atten.Constant"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Linear :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Atten.Linear"));
            theTechnique.Spot_Lights_Locations (GL.Types.UInt (index)).Atten.Exp :=
              Get_Uniform_Location (theTechnique, Spot_Name (index, "Atten.Exp"));
        end loop;
        return True;
    end Init;

    --  -------------------------------------------------------------------------

   procedure Init_Directional_Light (Light : out Direct_Light) is
    begin
        Light.Base.Colour := (1.0, 1.0, 1.0);
        Light.Base.Ambient_Intensity := 0.2;
        Light.Base.Diffuse_Intensity := 0.8;
        Light.Direction := (1.0, 0.0, 0.0);
    end Init_Directional_Light;

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

    procedure Set_Colour_Texture_Unit (theTechnique : in out Technique;
                                       Texture_Unit : GL.Types.Int) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Colour_Map_Location, Texture_Unit);
    end Set_Colour_Texture_Unit;

    --  -------------------------------------------------------------------------

   procedure Set_Directional_Light (theTechnique : in out Technique; Light : Direct_Light) is
      use GL.Uniforms;
    begin
      Set_Single (theTechnique.Direct_Light_Location.Color, Light.Base.Colour);
      Set_Single (theTechnique.Direct_Light_Location.Ambient_Intensity,
                  Light.Base.Ambient_Intensity);
      Set_Single (theTechnique.Direct_Light_Location.Diffuse_Intensity,
                  Light.Base.Diffuse_Intensity);
      Set_Single (theTechnique.Direct_Light_Location.Direction,
                  Maths.Normalized (Light.Direction));

    exception
      when others =>
         Put_Line ("An exception occurred in Lighting_Technique_26.Set_Directional_Light.");
         raise;
    end Set_Directional_Light;

    --  -------------------------------------------------------------------------

    procedure Set_Eye_World_Position (theTechnique : in out Technique;
                                      Position : GL.Types.Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Eye_World_Pos_Location, Position);
    end Set_Eye_World_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Light_WVP_Position (theTechnique : in out Technique;
                                      Position : GL.Types.Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (theTechnique.WVP_Location, Position);
    end Set_Light_WVP_Position;

    --  -------------------------------------------------------------------------

   procedure Set_Mat_Specular_Intensity (theTechnique : in out Technique;
                                        Intensity : GL.Types.Single) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Mat_Specular_Intensity_Location, Intensity);
    end Set_Mat_Specular_Intensity;

    --  -------------------------------------------------------------------------

   procedure Set_Mat_Specular_Power (theTechnique : in out Technique;
                                     Power : GL.Types.Single) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Mat_Specular_Power_Location, Power);
    end Set_Mat_Specular_Power;

    --  -------------------------------------------------------------------------

    procedure Set_Normal_Map_Texture_Unit (theTechnique : in out Technique;
                                           Texture_Unit : GL.Types.Int) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Normal_Map_Location, Texture_Unit);
    end Set_Normal_Map_Texture_Unit;

    --  -------------------------------------------------------------------------

   procedure Set_Point_Lights (theTechnique : in out Technique;
                               Lights : Point_Lights_Array) is
    use GL.Uniforms;
    begin
      Set_Int (theTechnique.Num_Point_Lights_Location, Point_Lights_Array'Size);
      for index in GL.Types.UInt range 1 .. Point_Lights_Array'Size loop
         Set_Single (theTechnique.Point_Lights_Locations (index).Color,
                                 Lights (index).Base.Colour);
         Set_Single (theTechnique.Point_Lights_Locations (index).Ambient_Intensity,
                                 Lights (index).Base.Ambient_Intensity);
         Set_Single (theTechnique.Point_Lights_Locations (index).Diffuse_Intensity,
                                 Lights (index).Base.Diffuse_Intensity);
         Set_Single (theTechnique.Point_Lights_Locations (index).Position,
                                 Lights (index).Position);
         Set_Single (theTechnique.Point_Lights_Locations (index).Atten.Constant_Atten,
                                 Lights (index).Atten.Constant_Atten);
         Set_Single (theTechnique.Point_Lights_Locations (index).Atten.Linear,
                                 Lights (index).Atten.Linear);
         Set_Single (theTechnique.Point_Lights_Locations (index).Atten.Exp,
                                 Lights (index).Atten.Exp);
      end loop;
    end Set_Point_Lights;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Map_Texture_Unit (theTechnique : in out Technique;
                                           Texture_Unit : GL.Types.Int) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Shadow_Map_Location, Texture_Unit);
    end Set_Shadow_Map_Texture_Unit;

    --  -------------------------------------------------------------------------

   procedure Set_Spot_Lights (theTechnique : in out Technique;
                              Lights       : Spot_Lights_Array) is
      use GL.Uniforms;
      use Maths.Single_Math_Functions;
    begin
      GL.Uniforms.Set_Int (theTechnique.Num_Spot_Lights_Location, Spot_Lights_Array'Size);
      for index in GL.Types.UInt range 1 .. Spot_Lights_Array'Size loop
         Set_Single (theTechnique.Spot_Lights_Locations (index).Color,
                                 Lights (index).Point.Base.Colour);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Ambient_Intensity,
                                 Lights (index).Point.Base.Ambient_Intensity);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Diffuse_Intensity,
                                 Lights (index).Point.Base.Diffuse_Intensity);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Position,
                                 Lights (index).Point.Position);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Direction,
                                Maths.Normalized (Lights (index).Direction));
         Set_Single (theTechnique.Spot_Lights_Locations (index).Cutoff,
                                 Cos (GL.Types.Single (Maths.Radians (Lights (index).Cutoff))));
         Set_Single (theTechnique.Spot_Lights_Locations (index).Atten.Constant_Atten,
                                 Lights (index).Point.Atten.Constant_Atten);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Atten.Linear,
                                 Lights (index).Point.Atten.Linear);
         Set_Single (theTechnique.Spot_Lights_Locations (index).Atten.Exp,
                                 Lights (index).Point.Atten.Exp);
      end loop;
    end Set_Spot_Lights;

    --  -------------------------------------------------------------------------

    procedure Set_World_Matrix (theTechnique : in out Technique;
                                WVP : GL.Types.Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (theTechnique.World_Matrix_Location, WVP);
    end Set_World_Matrix;

    --  -------------------------------------------------------------------------

   procedure Set_WVP (theTechnique : in out Technique;
                      WVP : GL.Types.Singles.Matrix4) is
    begin
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
