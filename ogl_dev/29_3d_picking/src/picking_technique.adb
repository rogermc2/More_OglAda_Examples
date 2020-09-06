
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders.Lists;

with Program_Loader;
with Utilities;


package body Picking_Technique is

   function Get_Draw_Index_Location (theTechnique : Pick_Technique)
                                     return GL.Uniforms.Uniform is
   begin
      return theTechnique.Draw_Index_Location;
   end Get_Draw_Index_Location;

   --  -------------------------------------------------------------------------

   function Get_Object_Index_Location (theTechnique : Pick_Technique)
                                       return GL.Uniforms.Uniform is
   begin
      return theTechnique.Object_Index_Location;
   end Get_Object_Index_Location;

   --  -------------------------------------------------------------------------

   function Get_WVP_Location (theTechnique : Pick_Technique)
                              return GL.Uniforms.Uniform is
   begin
      return theTechnique.WVP_Location;
   end Get_WVP_Location;

   --  -------------------------------------------------------------------------

   procedure Init (theTechnique : in out Pick_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      OK        : Boolean;
   begin
      --  Program_From includes linking
      theTechnique.Picking_Program := Program_From
        ((Src ("src/shaders/picking_29.vs", Vertex_Shader),
         Src ("src/shaders/picking_29.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (theTechnique.Picking_Program);
      if not OK then
         Put_Line ("Picking_Technique.Init, Picking_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Picking_Program));
      end if;

      Use_Program (theTechnique.Picking_Program);
      Utilities.Set_Uniform_Location (theTechnique.Picking_Program, "gWVP",
                                      theTechnique.WVP_Location);
      Utilities.Set_Uniform_Location (theTechnique.Picking_Program, "gDrawIndex",
                                      theTechnique.Draw_Index_Location);
      Utilities.Set_Uniform_Location (theTechnique.Picking_Program, "gObjectIndex",
                                      theTechnique.Object_Index_Location);
      Utilities.Show_Shader_Program_Data (theTechnique.Picking_Program);
   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Draw_Index (theTechnique : Pick_Technique;
                             Draw_Index   : GL.Types.UInt) is
   begin
      GL.Uniforms.Set_UInt (theTechnique.Draw_Index_Location, Draw_Index);
   end Set_Draw_Index;

   --  -------------------------------------------------------------------------

   procedure Set_Object_Index (theTechnique : Pick_Technique;
                               Object_Index : GL.Types.UInt) is
   begin
      GL.Uniforms.Set_UInt (theTechnique.Object_Index_Location, Object_Index);
   end Set_Object_Index;

   --  -------------------------------------------------------------------------

   procedure Set_WVP (theTechnique : Pick_Technique;
                      WVP          : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (theTechnique.WVP_Location, WVP);
   end Set_WVP;

   --  -------------------------------------------------------------------------

   function Picking_Program  (theTechnique : Pick_Technique)
                              return GL.Objects.Programs.Program is
   begin
      return theTechnique.Picking_Program;
   end Picking_Program;

   --  -------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Pick_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
      Shaders_List : GL.Objects.Shaders.Lists.List :=
                       GL.Objects.Programs.Attached_Shaders (theTechnique.Picking_Program);
      Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Picking_Program) then
         if Curs = GL.Objects.Shaders.Lists.No_Element then
            Put_Line ("Picking_Technique.Use_Program, Shaders list is empty");
         else
            GL.Objects.Programs.Use_Program (theTechnique.Picking_Program);
         end if;
      else
         Put_Line ("Picking_Technique.Use_Program Picking_Program link check failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Technique.Picking_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------

end Picking_Technique;
