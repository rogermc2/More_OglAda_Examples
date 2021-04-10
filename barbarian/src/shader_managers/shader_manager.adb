
with Ada.Containers.Vectors;

with GL.Objects.Shaders;

with GL.Uniforms;
with Program_Loader;

with Shader_Attributes;

package body Shader_Manager is
   use  GL.Uniforms;
   use GL.Objects.Programs;

   type Shader_Program_Data is record
      Shader_Program        : Program;   --  Sp
      Model_Matrix_ID       : GL.Uniforms.Uniform := 0;
      Projection_Matrix_ID  : GL.Uniforms.Uniform := 0;
      View_Matrix_ID        : GL.Uniforms.Uniform := 0;
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

   procedure Init_Fallback (Fallback_Shader : out GL.Objects.Programs.Program);

   --  -------------------------------------------------------------------------

   procedure Init (Fallback_Shader : out GL.Objects.Programs.Program) is
   begin
      Init_Fallback (Fallback_Shader);
   end Init;

   --  -------------------------------------------------------------------------

   procedure Init_Fallback (Fallback_Shader : out GL.Objects.Programs.Program) is
      use GL.Objects.Shaders;
      use Program_Loader;
      SP_Data : Shader_Program_Data;
   begin
      SP_Data.Shader_Program:= Program_From
        ((Src ("src/shaders_3_2/fallback_410.vert", Vertex_Shader),
         Src ("src/shaders_3_2/fallback_410.frag", Fragment_Shader)));

      SP_Data.Model_Matrix_ID := Uniform_Location (SP_Data.Shader_Program, "M");
      SP_Data.Projection_Matrix_ID := Uniform_Location (SP_Data.Shader_Program, "P");
      SP_Data.View_Matrix_ID := Uniform_Location (SP_Data.Shader_Program, "V");

      Use_Program (SP_Data.Shader_Program);
      GL.Uniforms.Set_Single (SP_Data.Model_Matrix_ID, Singles.Identity4);
      GL.Uniforms.Set_Single (SP_Data.Projection_Matrix_ID, Singles.Identity4);
      GL.Uniforms.Set_Single (SP_Data.View_Matrix_ID, Singles.Identity4);

      Fallback_Shader_Programs.Append (SP_Data);
      Fallback_Shader := SP_Data.Shader_Program;

   end Init_Fallback;

   --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Program_Index : Positive;
                               Model_Matrix  : Singles.Matrix4) is
      SP_Data : constant Shader_Program_Data :=
                  Fallback_Shader_Programs.Element (Program_Index);
   begin
      GL.Uniforms.Set_Single (SP_Data.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Projection_Matrix (Program_Index : Positive;
                                    Projection_Matrix: Singles.Matrix4) is
      SP_Data : constant Shader_Program_Data :=
                  Fallback_Shader_Programs.Element (Program_Index);
   begin
      GL.Uniforms.Set_Single (SP_Data.Projection_Matrix_ID, Projection_Matrix);
   end Set_Projection_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_View_Matrix (Program_Index : Positive;
                               View_Matrix  : Singles.Matrix4) is
      SP_Data : constant Shader_Program_Data :=
                  Fallback_Shader_Programs.Element (Program_Index);
   begin
      GL.Uniforms.Set_Single (SP_Data.View_Matrix_ID, View_Matrix);
   end Set_View_Matrix;

   --  -------------------------------------------------------------------------

end Shader_Manager;
