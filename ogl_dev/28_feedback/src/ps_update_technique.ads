
with Ada.Strings.Unbounded;

with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package PS_Update_Technique is

   type Update_Technique is private;

   Varyings : constant array (1 .. 4) of Ada.Strings.Unbounded.Unbounded_String :=
    (Ada.Strings.Unbounded.To_Unbounded_String ("Type1"),
    Ada.Strings.Unbounded.To_Unbounded_String ("Position1"),
    Ada.Strings.Unbounded.To_Unbounded_String ("Velocity1"),
    Ada.Strings.Unbounded.To_Unbounded_String ("Age1"));

   function Get_Random_Texture_Location (theTechnique : Update_Technique)
                                         return GL.Uniforms.Uniform;
   function Get_Time_Location (theTechnique : Update_Technique)
                               return GL.Uniforms.Uniform;
   function Get_Update_Program (theTechnique : Update_Technique)
                                return GL.Objects.Programs.Program;
   procedure Init (theTechnique : in out Update_Technique);
   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                                 Delta_Time : GL.Types.Int);
   procedure Set_Time (theTechnique : Update_Technique;
                       theTime : GL.Types.Int);
   procedure Set_Random_Texture_Unit (theTechnique : Update_Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_Launcher_Lifetime (theTechnique : Update_Technique;
                                    Lifetime : GL.Types.Single);
   procedure Set_Shell_Lifetime (theTechnique : Update_Technique;
                                 Lifetime : GL.Types.Single);
   procedure Set_Secondary_Shell_Lifetime (theTechnique : Update_Technique;
                                           Lifetime : GL.Types.Single);
   procedure Use_Program (theTechnique : Update_Technique);

private
    use GL.Uniforms;
   type Update_Technique is record
      --  Shader_Object_List -- inherited from Technique.h
      Update_Program                     : GL.Objects.Programs.Program;
      Delta_Millisec_Location            : GL.Uniforms.Uniform := -1;
      Random_Texture_Location            : GL.Uniforms.Uniform := -1;
      Time_Location                      : GL.Uniforms.Uniform := -1;
      Launcher_Lifetime_Location         : GL.Uniforms.Uniform := -1;
      Shell_Lifetime_Location            : GL.Uniforms.Uniform := -1;
      Secondary_Shell_Lifetime_Location  : GL.Uniforms.Uniform := -1;
   end record;

end PS_Update_Technique;
