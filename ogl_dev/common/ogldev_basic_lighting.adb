
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Gl.Types.Colors;

with Maths;
with Program_Loader;
with Utilities;

package body Ogldev_Basic_Lighting is
   use GL.Uniforms;

   function Colour_To_Vec3 (theColour : Colors.Basic_Color) return Singles.Vector3 is
      Col         : constant Colors.Basic_Color := theColour;
      Vec3_Colour : constant Singles.Vector3 :=
                          (Col (Colors.R), Col (Colors.G), Col (Colors.B));
   begin
      return Vec3_Colour;
   end Colour_To_Vec3;

   function Init (Lighting_Technique : in out Basic_Lighting_Technique) return Boolean is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      use Utilities;

      function Point_Name (Index : Int; Unif : String) return String is
      begin
         return To_String ("gPointLights[" &
                             Trim (To_Unbounded_String (Int'Image (Index - 1)), Left)
                           & "]." & Unif);
      end Point_Name;

      function Spot_Name (Index : Int; Unif : String) return String is
      begin
         return To_String ("gSpotLights[" &
                             Trim (To_Unbounded_String (Int'Image (Index - 1)), Left)
                           & "]." & Unif);
      end Spot_Name;

      Shader_Program : GL.Objects.Programs.Program;
      OK : Boolean;

   begin
      Lighting_Technique.Lighting_Program :=
        Program_From ((Src ("../common/shaders/basic_lighting.vs", Vertex_Shader),
                      Src ("../common/shaders/basic_lighting.fs", Fragment_Shader)));
      OK := GL.Objects.Programs.Link_Status (Lighting_Technique.Lighting_Program);
      if not OK then
         Put_Line ("Ogldev_Basic_Lighting.Init, Lighting_Program link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Lighting_Technique.Lighting_Program));
      end if;
--        else
         --  Can't get GL.Objects.Programs.Validate_Status to work.
         --           OK := GL.Objects.Programs.Validate_Status (Lighting_Technique.Lighting_Program);
         --           if not OK then
         --              Put_Line ("Ogldev_Basic_Lighting.Init, Lighting_Program validation failed");
         --              Put_Line ("Info log:");
         --              Put_Line (GL.Objects.Programs.Info_Log (Lighting_Technique.Lighting_Program));
         --           else
         --              Put_Line ("Ogldev_Basic_Lighting.Init, Lighting_Program validated");
         --           end if;
--        end if;

      if OK then
         Shader_Program := Lighting_Technique.Lighting_Program;
         Use_Program (Shader_Program);
         Set_Uniform_Location (Shader_Program, "gWVP",
                               Lighting_Technique.WVP_Location);
         Set_Uniform_Location (Shader_Program, "gWorld",
                               Lighting_Technique.World_Matrix_Location);
         Set_Uniform_Location (Shader_Program, "gColorMap",
                               Lighting_Technique.Colour_Texture_Location);
         Set_Uniform_Location (Shader_Program, "gEyeWorldPos",
                               Lighting_Technique.Eye_World_Pos_Location);

         Set_Uniform_Location (Shader_Program, "gDirectionalLight.Base.Color",
                               Lighting_Technique.Dir_Light_Location.Colour);
         Set_Uniform_Location (Shader_Program, "gDirectionalLight.Base.AmbientIntensity",
                               Lighting_Technique.Dir_Light_Location.Ambient_Intensity);
         Set_Uniform_Location (Shader_Program, "gDirectionalLight.Base.DiffuseIntensity",
                               Lighting_Technique.Dir_Light_Location.Diffuse_Intensity);
         Set_Uniform_Location (Shader_Program, "gDirectionalLight.Direction",
                               Lighting_Technique.Dir_Light_Location.Direction);

         Set_Uniform_Location (Shader_Program, "gMatSpecularIntensity",
                               Lighting_Technique.Mat_Specular_Intensity_Location);
         Set_Uniform_Location (Shader_Program, "gSpecularPower",
                               Lighting_Technique.Mat_Specular_Power_Location);
         Set_Uniform_Location (Shader_Program, "gNumPointLights",
                               Lighting_Technique.Num_Point_Lights_Location);
         Set_Uniform_Location (Shader_Program, "gNumSpotLights",
                               Lighting_Technique.Num_Spot_Lights_Location);

         for index in Lighting_Technique.Point_Lights_Location'Range loop
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Base.Color"),
                               Lighting_Technique.Point_Lights_Location (index).Colour);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Base.AmbientIntensity"),
                               Lighting_Technique.Point_Lights_Location (index).Ambient_Intensity);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Base.DiffuseIntensity"),
                               Lighting_Technique.Point_Lights_Location (index).Diffuse_Intensity);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Position"),
                               Lighting_Technique.Point_Lights_Location (index).Position);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Atten.Constant"),
                               Lighting_Technique.Point_Lights_Location (index).Attenuation.Atten_Constant);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Atten.Linear"),
                               Lighting_Technique.Point_Lights_Location (index).Attenuation.Linear);
         Set_Uniform_Location (Shader_Program, Point_Name (index, "Atten.Exp"),
                               Lighting_Technique.Point_Lights_Location (index).Attenuation.Exp);
         end loop;

         for index in Lighting_Technique.Spot_Lights_Location'Range loop
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Point.Base.Color"),
                               Lighting_Technique.Spot_Lights_Location (index).Colour);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Point.Base.AmbientIntensity"),
                               Lighting_Technique.Spot_Lights_Location (index).Ambient_Intensity);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Point.Base.DiffuseIntensity"),
                               Lighting_Technique.Spot_Lights_Location (index).Diffuse_Intensity);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Point.Position"),
                               Lighting_Technique.Spot_Lights_Location (index).Position);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Direction"),
                               Lighting_Technique.Spot_Lights_Location (index).Direction);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Cutoff"),
                               Lighting_Technique.Spot_Lights_Location (index).Cut_Off);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Base.Atten.Constant"),
                               Lighting_Technique.Spot_Lights_Location (index).Attenuation.Atten_Constant);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Base.Atten.Linear"),
                               Lighting_Technique.Spot_Lights_Location (index).Attenuation.Linear);
         Set_Uniform_Location (Shader_Program, Spot_Name (index, "Base.Atten.Exp"),
                               Lighting_Technique.Spot_Lights_Location (index).Attenuation.Exp);
         end loop;
      end if;
      return OK;

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Lighting.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   function Lighting_Program (Technique : Basic_Lighting_Technique)
                              return GL.Objects.Programs.Program is
   begin
      return Technique.Lighting_Program;
   end Lighting_Program;

   --  -------------------------------------------------------------------------

   procedure Set_Color_Texture_Unit (Technique    : Basic_Lighting_Technique;
                                     Texture_Unit : GL.Types.UInt) is
   begin
      Set_Int (Technique.Colour_Texture_Location,  GL.Types.Int (Texture_Unit));
   end Set_Color_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Directional_Light (Technique : Basic_Lighting_Technique;
                                    Light     : Directional_Light) is
      Light_Direction : Singles.Vector3 := Direction (Light);
   begin
      Light_Direction := Maths.Normalized (Light_Direction);
      Set_Single (Technique.Dir_Light_Location.Direction, Direction (Light));
      Set_Single (Technique.Dir_Light_Location.Colour,
                  Colour_To_Vec3 (Colour (Light)));
      Set_Single (Technique.Dir_Light_Location.Ambient_Intensity,
                  Ambient_Intensity (Light));
      Set_Single (Technique.Dir_Light_Location.Direction, Light_Direction);
      Set_Single (Technique.Dir_Light_Location.Diffuse_Intensity,
                  Diffuse_Intensity (Light));
   end Set_Directional_Light;

   --  -------------------------------------------------------------------------

   procedure Set_Point_Lights (Technique : Basic_Lighting_Technique;
                               Lights    : Point_Light_Array) is
      Num_Lights : constant Int :=  Lights'Length;
      Location   : Point_Light_Locations;
   begin
      GL.Uniforms.Set_Int (Technique.Num_Point_Lights_Location, Num_Lights);
      for index in UInt range Lights'First .. Lights'Last loop
         Location := Technique.Point_Lights_Location (Int (index));
         Set_Single (Location.Colour,
                     Colour_To_Vec3 (Colour (Lights (index))));
         Set_Single (Location.Ambient_Intensity, Ambient_Intensity (Lights (index)));
         Set_Single (Location.Diffuse_Intensity, Diffuse_Intensity (Lights (index)));
         Set_Single (Location.Position, Position (Lights (index)));
         Set_Single (Location.Attenuation.Atten_Constant,
                     Attenuation_Constant (Lights (index)));
         Set_Single (Location.Attenuation.Linear, Attenuation_Linear (Lights (index)));
         Set_Single (Location.Attenuation.Exp, Attenuation_Exponent (Lights (index)));
       end loop;

    exception
        when  others =>
            Put_Line ("An exception occurred in Ogldev_Basic_Lighting.Set_Point_Lights.");
            raise;
   end Set_Point_Lights;

   --  -------------------------------------------------------------------------

   procedure Set_Spot_Lights (Technique : Basic_Lighting_Technique;
                              Spots     : Ogldev_Lights_Common.Spot_Light_Array) is
      use Maths.Single_Math_Functions;
      Num_Lights      : constant Int :=  Spots'Length;
      Location        : Spot_Light_Locations;
      Spot            : Spot_Light;
      Light_Direction : Singles.Vector3;
   begin
      GL.Uniforms.Set_Int (Technique.Num_Spot_Lights_Location, Num_Lights);
      for index in UInt range Spots'First .. Spots'Last loop
         Spot := Spots (index);
         Light_Direction := Maths.Normalized (Direction (Spot));
         Location := Technique.Spot_Lights_Location (Int (index));
         Set_Single (Location.Colour, Colour_To_Vec3 (Colour (Spot)));
         Set_Single (Location.Ambient_Intensity, Ambient_Intensity (Spot));
         Set_Single (Location.Diffuse_Intensity, Diffuse_Intensity (Spot));
         Set_Single (Location.Position, Position (Spot));
         Set_Single (Location.Direction, Light_Direction);
         Set_Single (Location.Cut_Off, Cos (Single (Maths.Radians (Cut_Off (Spot)))));
         Set_Single (Location.Attenuation.Atten_Constant,
                     Attenuation_Constant (Spot));
         Set_Single (Location.Attenuation.Linear, Attenuation_Linear (Spot));
         Set_Single (Location.Attenuation.Exp, Exponent (Spot));
      end loop;

   end Set_Spot_Lights;

   --  -------------------------------------------------------------------------

   procedure Set_Spot_Light (Technique : Basic_Lighting_Technique;
                             Spot      : Ogldev_Lights_Common.Spot_Light) is
      use Maths.Single_Math_Functions;
      Location        : Spot_Light_Locations;
      Light_Direction : Singles.Vector3 := Direction (Spot);
   begin
      Light_Direction := Maths.Normalized (Light_Direction);
      GL.Uniforms.Set_Int (Technique.Num_Spot_Lights_Location, 1);
      Location := Technique.Spot_Lights_Location (1);
      Set_Single (Location.Colour, Colour_To_Vec3 (Colour (Spot)));
      Set_Single (Location.Ambient_Intensity, Ambient_Intensity (Spot));
      Set_Single (Location.Diffuse_Intensity, Diffuse_Intensity (Spot));
      Set_Single (Location.Direction, Light_Direction);
      Set_Single (Location.Cut_Off, Cos (Single (Maths.Radians (Cut_Off (Spot)))));
   end Set_Spot_Light;

   --  -------------------------------------------------------------------------

   procedure Set_Eye_World_Pos (Technique :  Basic_Lighting_Technique;
                                Eye_Position : Singles.Vector3) is
   begin
      Set_Single (Technique.Eye_World_Pos_Location, Eye_Position);
   end Set_Eye_World_Pos;

   --  -------------------------------------------------------------------------

   procedure Set_Mat_Specular_Intensity (Technique : Basic_Lighting_Technique;
                                         Intensity : Single) is
   begin
      Set_Single (Technique.Mat_Specular_Intensity_Location, Intensity);
   end  Set_Mat_Specular_Intensity;

   --  -------------------------------------------------------------------------

   procedure Set_Mat_Specular_Power (Technique : Basic_Lighting_Technique;
                                     Power     : UInt) is
   begin
      Set_Single (Technique.Mat_Specular_Power_Location, Single (Power));
   end Set_Mat_Specular_Power;

   --  -------------------------------------------------------------------------

   procedure Set_World_Matrix (Technique     : Basic_Lighting_Technique;
                               World_Inverse : Singles.Matrix4) is
   begin
      Set_Single (Technique.World_Matrix_Location, World_Inverse);
   end Set_World_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_WVP (Technique : Basic_Lighting_Technique;
                      WVP       : Singles.Matrix4) is
   begin
      Set_Single (Technique.WVP_Location, WVP);
   end Set_WVP;

   --  -------------------------------------------------------------------------

end Ogldev_Basic_Lighting;
