
with Ada.Containers.Vectors;

with GL.Objects.Shaders;

with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;
with Program_Loader;

with Shader_Attributes;

package body Shader_Manager is
   use  GL.Uniforms;
   use GL.Objects.Programs;

   package Shader_Uniforms_Package is new
     Ada.Containers.Vectors (Positive, Uniform);
   type Shader_Uniforms_List is new Shader_Uniforms_Package.Vector with null Record;

   type Shader_Program_Data is record
      Shader_Uniforms : Shader_Uniforms_List;
      Shader_Program  : Program;   --  Sp
--        Vertex_Shader   : Shader;    --  Vs
--        Fragment_Shader : Shader;    --  Fs
--        Uniform_Count   : Int := 0;
--        All_Compiled    : Boolean := False;
--        Linked          : Boolean := False;
   end record;

   package Shader_Programs_Package is new
     Ada.Containers.Vectors (Positive, Shader_Program_Data);
   type Shader_Programs_List is new Shader_Programs_Package.Vector with null Record;

   Fallback_Shader_Programs  : Shader_Programs_List;
   Fallback_Uniforms         : Shader_Uniforms_List;

   procedure Init_Fallback;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Init_Fallback;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Init_Fallback is
      use GL.Objects.Shaders;
      use Program_Loader;
      SP_Data : Shader_Program_Data;
      SU_List : Shader_Uniforms_List;
   begin
      SP_Data.Shader_Program:= Program_From
        ((Src ("src/shaders_3_2/fallback_410.vert", Vertex_Shader),
         Src ("src/shaders_3_2/fallback_410.frag", Fragment_Shader)));

      Bind_Attrib_Location (SP_Data.Shader_Program,
                            Shader_Attributes.Attrib_VP, "vp");

      SU_List.Append (Uniform_Location (SP_Data.Shader_Program, "P"));
      SU_List.Append (Uniform_Location (SP_Data.Shader_Program, "V"));
      SU_List.Append (Uniform_Location (SP_Data.Shader_Program, "M"));
      SP_Data.Shader_Uniforms := SU_List;

      Fallback_Shader_Programs.Append (SP_Data);

   end Init_Fallback;

   --  -------------------------------------------------------------------------

end Shader_Manager;
