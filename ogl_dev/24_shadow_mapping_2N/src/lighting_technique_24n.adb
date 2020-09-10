
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

package body Lighting_Technique_24N is 
    
    function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
    function Point_Name (Index : GL.Types.Int; Unif : String) return String;
    function Spot_Name (Index : GL.Types.Int; Unif : String) return String;
    
    --  -------------------------------------------------------------------------
    
    function Get_Directional_Ambient (Light : Directional_Light) return Single is
    begin
        return Light.Base.Ambient_Intensity;
    end Get_Directional_Ambient;

    --  -------------------------------------------------------------------------
    
    function Get_Directional_Diffuse (Light : Directional_Light) return Single is
    begin
        return Light.Base.Diffuse_Intensity;
    end Get_Directional_Diffuse;

    --  -------------------------------------------------------------------------
    
    function Get_Position (Light : Spot_Light) return Singles.Vector3 is
    begin
        return Light.Point.Position;
    end Get_Position;

    --  -------------------------------------------------------------------------
    
    function Get_Direction (Light : Spot_Light) return Singles.Vector3 is
    begin
        return Light.Direction;
    end Get_Direction;

    --  -------------------------------------------------------------------------
    
    function Get_Spot_Ambient (Light : Spot_Light) return Single is
    begin
        return Light.Point.Base.Ambient_Intensity;
    end Get_Spot_Ambient;

    --  -------------------------------------------------------------------------
    
    function Get_Spot_Diffuse (Light : Spot_Light) return Single is
    begin
        return Light.Point.Base.Diffuse_Intensity;
    end Get_Spot_Diffuse;

    --  -------------------------------------------------------------------------
    function Get_Uniform (theTechnique : Technique; Uniform_Name : String)
                          return GL.Uniforms.Uniform is
    begin
        return GL.Objects.Programs.Uniform_Location (Light_Program (theTechnique), Uniform_Name);
    end Get_Uniform;

    --  -------------------------------------------------------------------------

    function Init (theTechnique  : out Technique) return Boolean is
        use GL.Objects.Shaders;
        use Program_Loader;
        OK  : Boolean := False;
    begin
        theTechnique.Lighting_Program := Program_From
          ((Src ("src/shaders/lighting_24n.vs", Vertex_Shader),
           Src ("src/shaders/lighting_24n.fs", Fragment_Shader)));

        OK := GL.Objects.Programs.Link_Status (theTechnique.Lighting_Program);
        if not OK then
            Put_Line ("Build_Shader_Program, theTechnique.Lighting_Program Link failed");
            Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Lighting_Program));
        else
            GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
            theTechnique.WVP_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gWVP");
            theTechnique.Light_WVP_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gLightWVP");
            theTechnique.World_Matrix_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gWorld");
            theTechnique.Sampler_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gSampler");
            theTechnique.Shadow_Map_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gShadowMap");
            theTechnique.Eye_World_Pos_Uniform :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gEyeWorldPos");
            theTechnique.Direct_Light_Uniform.Colour := GL.Objects.Programs.Uniform_Location
              (theTechnique.Lighting_Program, "gDirectionalLight.Base.Color");
            theTechnique.Direct_Light_Uniform.Ambient_Intensity := GL.Objects.Programs.Uniform_Location
              (theTechnique.Lighting_Program, "gDirectionalLight.Base.AmbientIntensity");
            theTechnique.Direct_Light_Uniform.Diffuse_Intensity := GL.Objects.Programs.Uniform_Location
              (theTechnique.Lighting_Program, "gDirectionalLight.Base.DiffuseIntensity");
            theTechnique.Direct_Light_Uniform.Direction := GL.Objects.Programs.Uniform_Location
              (theTechnique.Lighting_Program, "gDirectionalLight.Direction");

            theTechnique.Mat_Specular_Intensity_Uniform  :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gMatSpecularIntensity");
            theTechnique.Mat_Specular_Power_Uniform  :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gSpecularPower");
            theTechnique.Num_Point_Lights_Uniform  :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gNumPointLights");
            theTechnique.Num_Spot_Lights_Uniform  :=
              GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gNumSpotLights");
            
            for index in GL.Types.Int range
              Point_Lights_Uniform_Array'First .. Point_Lights_Uniform_Array'Last loop
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Colour :=
                  Get_Uniform (theTechnique, Point_Name (index, "Base.Color"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Ambient_Intensity :=
                  Get_Uniform (theTechnique, Point_Name (index, "Base.AmbientIntensity"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Diffuse_Intensity :=
                  Get_Uniform (theTechnique, Point_Name (index, "Base.DiffuseIntensity"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Position :=
                  Get_Uniform (theTechnique, Point_Name (index, "Position"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Atten.Constant_Atten :=
                  Get_Uniform (theTechnique, Point_Name (index, "Atten.Constant"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Atten.Linear :=
                  Get_Uniform (theTechnique, Point_Name (index, "Atten.Linear"));
                theTechnique.Point_Uniforms (GL.Types.Int (index)).Atten.Exp :=
                  Get_Uniform (theTechnique, Point_Name (index, "Atten.Exp"));
            end loop;

            for index in GL.Types.Int range 
              Spot_Lights_Uniform_Array'First .. Spot_Lights_Uniform_Array'Last loop
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Colour :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Base.Color"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Ambient_Intensity :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Base.AmbientIntensity"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Diffuse_Intensity :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Base.DiffuseIntensity"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Position :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Position"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Atten.Constant_Atten :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Atten.Constant"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Atten.Linear :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Atten.Linear"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Atten.Exp :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Point.Atten.Exp"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Direction :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Direction"));
                theTechnique.Spot_Uniforms (GL.Types.Int (index)).Cutoff :=
                  Get_Uniform (theTechnique, Spot_Name (index, "Cutoff"));
            end loop;
        end if;
        return OK;

    exception
        when  others =>
            Put_Line ("An exception occurred in Lighting_Technique_24N.Init.");
            raise;
    end Init;
   
    --   -------------------------------------------------------------------------------------------------------

    procedure Init_Directional_Light (Light : out Directional_Light) is
    begin
        Light.Base.Colour := (1.0, 1.0, 1.0);
        Light.Base.Ambient_Intensity := 0.0;
        Light.Base.Diffuse_Intensity := 0.4;  --  0.1;
        Light.Direction := (1.0, -1.0, 0.0);
    end Init_Directional_Light;

    --  -------------------------------------------------------------------------
 
    function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program is
    begin
        return theTechnique.Lighting_Program;
    end Light_Program;

    --  -------------------------------------------------------------------------

    function Point_Name (Index : GL.Types.Int; Unif : String) return String is
        use Ada.Strings.Unbounded;
    begin
        return To_String ("gPointLights[" &
                            Trim (To_Unbounded_String (Int'Image (Index - 1)), Ada.Strings.Left)
                          & "]." & Unif);
    end Point_Name;

    --  -------------------------------------------------------------------------
    
    procedure Set_Ambient_Intensity (theTechnique : Technique; Intensity : Single) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Uniform.Ambient_Intensity, Intensity);
    end Set_Ambient_Intensity;
   
    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Directional_Ambient (Light : in out Directional_Light;
                                       Ambient : Single) is
    begin
        Light.Base.Ambient_Intensity := Ambient;
    end Set_Directional_Ambient;

    --  -------------------------------------------------------------------------
    
    procedure Set_Directional_Diffuse (Light : in out Directional_Light;
                                       Diffuse : Single) is
    begin
        Light.Base.Diffuse_Intensity := Diffuse;
    end Set_Directional_Diffuse;

    --  -------------------------------------------------------------------------
    
    procedure Set_Directional_Light_Uniform (theTechnique : Technique; 
                                             Light        : Directional_Light) is
        Direction : constant Singles.Vector3 := Maths.Normalized (Light.Direction);
    begin
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Uniform.Colour, Light.Base.Colour (GL.X), 
                                Light.Base.Colour (GL.Y), Light.Base.Colour (GL.Z));
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Uniform.Ambient_Intensity, Light.Base.Ambient_Intensity);
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Uniform.Direction, Direction);
        GL.Uniforms.Set_Single (theTechnique.Direct_Light_Uniform.Diffuse_Intensity, Light.Base.Diffuse_Intensity);
    end Set_Directional_Light_Uniform;
   
    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Eye_World_Pos_Uniform (theTechnique : Technique; Eye_World_Pos : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Eye_World_Pos_Uniform, Eye_World_Pos (GL.X),
                                Eye_World_Pos (GL.Y), Eye_World_Pos (GL.Z));
    end Set_Eye_World_Pos_Uniform;
   
    --   -------------------------------------------------------------------------------------------------------
   
    procedure Set_Light_WVP_Uniform (theTechnique : Technique; WVP : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Light_WVP_Uniform, WVP);    
    end Set_Light_WVP_Uniform;
   
    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Mat_Specular_Intensity (theTechnique : Technique; Intensity : Single) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Mat_Specular_Intensity_Uniform, Intensity);
    end Set_Mat_Specular_Intensity;

    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Mat_Specular_Power (theTechnique : Technique; Power : Single) is
    begin
        GL.Uniforms.Set_Single (theTechnique.Mat_Specular_Power_Uniform, Power);
    end Set_Mat_Specular_Power;
    
    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Point_Light (Light : in out Point_Light; Ambient, Diffuse : Single; 
                               Colour, Pos : Singles.Vector3; Atten : Attenuation) is
    begin
        Light.Base.Colour := Colour;
        Light.Base.Ambient_Intensity := Ambient;
        Light.Base.Diffuse_Intensity := Diffuse;
        Light.Position := Pos;
        Light.Atten := Atten;
    end Set_Point_Light;

    --  -------------------------------------------------------------------------   
 
    procedure Set_Point_Light_Uniforms (theTechnique : Technique; Lights : Point_Lights_Array) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Num_Point_Lights_Uniform, Max_Point_Lights);
        for index in GL.Types.Int range Lights'First .. Lights'Last loop
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Colour,
                                    Lights (index).Base.Colour);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Ambient_Intensity,
                                    Lights (index).Base.Ambient_Intensity);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Diffuse_Intensity,
                                    Lights (index).Base.Diffuse_Intensity);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Position,
                                    Lights (index).Position);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Atten.Constant_Atten,
                                    Lights (index).Atten.Constant_Atten);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Atten.Linear,
                                    Lights (index).Atten.Linear);
            GL.Uniforms.Set_Single (theTechnique.Point_Uniforms (index).Atten.Exp,
                                    Lights (index).Atten.Exp);
        end loop;                        
    end Set_Point_Light_Uniforms;

    --   -------------------------------------------------------------------------------------------------------

    procedure Set_Shadow_Texture_Unit (theTechnique : Technique; Texture_Unit : Int) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Shadow_Map_Uniform, Texture_Unit);
    end Set_Shadow_Texture_Unit;
   
    --   -----------------------------------------------------------------------------
    
    procedure Set_Spot_Ambient (Light : in out Spot_Light; Ambient : Single) is
    begin
        Light.Point.Base.Ambient_Intensity := Ambient;
    end Set_Spot_Ambient;

    --  -------------------------------------------------------------------------

    procedure Set_Spot_Diffuse (Light : in out Spot_Light; Diffuse : Single) is
    begin
        Light.Point.Base.Diffuse_Intensity := Diffuse;
    end Set_Spot_Diffuse;

    --  -------------------------------------------------------------------------

    procedure Set_Spot_Light (Light : in out Spot_Light; Ambient, Diffuse : Single; 
                              Colour, Pos, Direction : Singles.Vector3; 
                              Atten : Attenuation; Cut_Off : Maths.Degree) is
    begin
        Light.Point.Base.Colour := Colour;
        Light.Point.Base.Ambient_Intensity := Ambient;
        Light.Point.Base.Diffuse_Intensity := Diffuse;
        Light.Point.Position := Pos;
        Light.Direction := Direction;
        Light.Point.Atten := Atten;
        Light.Cutoff := Cut_Off;
    end Set_Spot_Light;

    --  -------------------------------------------------------------------------
   
    procedure Set_Spot_Light_Uniforms (theTechnique : Technique; 
                                       Lights : Spot_Lights_Array) is
        use Maths.Single_Math_Functions;
    begin
        GL.Uniforms.Set_Int (theTechnique.Num_Spot_Lights_Uniform, Max_Spot_Lights);
        for index in GL.Types.Int range Lights'First .. Lights'Last loop
            GL.Uniforms.Set_Single (theTechnique.Spot_Uniforms (index).Colour,
                                    Lights (index).Point.Base.Colour);
            GL.Uniforms.Set_Single
              (theTechnique.Spot_Uniforms (index).Ambient_Intensity,
               Lights (index).Point.Base.Ambient_Intensity);
            GL.Uniforms.Set_Single 
              (theTechnique.Spot_Uniforms (index).Diffuse_Intensity,
               Lights (index).Point.Base.Diffuse_Intensity);
            GL.Uniforms.Set_Single (theTechnique.Spot_Uniforms (index).Position,
                                    Lights (index).Point.Position);
            GL.Uniforms.Set_Single (theTechnique.Spot_Uniforms (index).Direction,
                                    Maths.Normalized (Lights (index).Direction));
            GL.Uniforms.Set_Single 
              (theTechnique.Spot_Uniforms (index).Cutoff,
               Cos (Single (Maths.To_Radians (Lights (index).Cutoff))));
            GL.Uniforms.Set_Single
              (theTechnique.Spot_Uniforms (index).Atten.Constant_Atten,
               Lights (index).Point.Atten.Constant_Atten);
            GL.Uniforms.Set_Single (theTechnique.Spot_Uniforms (index).Atten.Linear,
                                    Lights (index).Point.Atten.Linear);
            GL.Uniforms.Set_Single (theTechnique.Spot_Uniforms (index).Atten.Exp,
                                    Lights (index).Point.Atten.Exp);
        end loop;
    end Set_Spot_Light_Uniforms;

    --  -------------------------------------------------------------------------
  
    procedure Set_Texture_Unit (theTechnique : Technique; Texture_Unit : Int) is
    begin
        GL.Uniforms.Set_Int (theTechnique.Sampler_Uniform, Texture_Unit);
    end Set_Texture_Unit;
   
    --   -----------------------------------------------------------------------------
  
    procedure Set_World_Matrix_Uniform (theTechnique  : Technique;
                                        World_Inverse : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (theTechnique.World_Matrix_Uniform, World_Inverse);
    end Set_World_Matrix_Uniform;
   
    --   -------------------------------------------------------------------------------------------------------

    procedure Set_WVP_Uniform (theTechnique : Technique; WVP : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (theTechnique.WVP_Uniform, WVP);    
    end Set_WVP_Uniform;
   
    --   -------------------------------------------------------------------------------------------------------

    function Spot_Name (Index : GL.Types.Int; Unif : String) return String is
        use Ada.Strings.Unbounded;
    begin
        return To_String ("gSpotLights[" &
                            Trim (To_Unbounded_String (Int'Image (Index - 1)), Ada.Strings.Left)
                          & "]." & Unif);
    end Spot_Name;

    --  -------------------------------------------------------------------------

    procedure Use_Program (theTechnique : Technique) is
        use GL.Objects.Shaders.Lists;
    begin
        if GL.Objects.Programs.Link_Status (theTechnique.Lighting_Program) then
            declare
                Shaders_List : constant GL.Objects.Shaders.Lists.List :=
                                 GL.Objects.Programs.Attached_Shaders (theTechnique.Lighting_Program);
                Curs         : constant GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
            begin
                if Curs = GL.Objects.Shaders.Lists.No_Element then
                    Put_Line ("Lighting_Technique_24N.Use_Program, Shaders list is empty");
                else
                    GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
                end if;
            end;  -- declare block
        else
            Put_Line ("Lighting_Technique_24N.Use_Program theTechnique Link_Status check failed.");
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Lighting_Technique_24N.Use_Program.");
            raise;
    end Use_Program;

    --  -------------------------------------------------------------------------
    
end Lighting_Technique_24N;
