
with Ada.Strings.Unbounded;

with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Picking_Technique is

   type Pick_Technique is private;

   function Get_Draw_Index_Location (theTechnique : Pick_Technique)
                                       return GL.Uniforms.Uniform;
   function Get_Object_Index_Location (theTechnique : Pick_Technique)
                                        return GL.Uniforms.Uniform;
   function Get_WVP_Location (theTechnique : Pick_Technique)
                               return GL.Uniforms.Uniform;
   procedure Init (theTechnique : in out Pick_Technique);
   function Picking_Program  (theTechnique : Pick_Technique)
                               return GL.Objects.Programs.Program;
   procedure Set_Draw_Index (theTechnique : Pick_Technique;
                             Draw_Index   : GL.Types.UInt);
   procedure Set_Object_Index (theTechnique : Pick_Technique;
                               Object_Index : GL.Types.UInt);
   procedure Set_WVP (theTechnique : Pick_Technique;
                      WVP          : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Pick_Technique);

private
   use GL.Uniforms;
   type Pick_Technique is record
      Picking_Program       : GL.Objects.Programs.Program;
      WVP_Location          : GL.Uniforms.Uniform := -1;
      Draw_Index_Location   : GL.Uniforms.Uniform := -1;
      Object_Index_Location : GL.Uniforms.Uniform := -1;
   end record;

end Picking_Technique;
