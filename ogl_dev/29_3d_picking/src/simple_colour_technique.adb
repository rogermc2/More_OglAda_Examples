
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders.Lists;

with Program_Loader;
with Utilities;

package body Simple_Colour_Technique is

   function Get_WVP_Location (theTechnique : Colour_Technique)
                              return GL.Uniforms.Uniform is
   begin
      return theTechnique.WVP_Location;
   end Get_WVP_Location;

   --  -------------------------------------------------------------------------

   procedure Init (theTechnique : in out Colour_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      OK        : Boolean;
   begin
      --  Program_From includes linking
      theTechnique.Colour_Program := Program_From
        ((Src ("src/shaders/simple_colour_29.vs", Vertex_Shader),
          Src ("src/shaders/simple_colour_29.fs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (theTechnique.Colour_Program);
      if not OK then
         Put_Line ("Simple_Colour_Technique.Init, Colour_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Colour_Program));
      end if;

      Utilities.Set_Uniform_Location (theTechnique.Colour_Program, "gWVP",
                                      theTechnique.WVP_Location);
    exception
      when  others =>
         Put_Line ("An exception occurred in Simple_Colour_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_WVP (theTechnique : Colour_Technique;
                      WVP : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (theTechnique.WVP_Location, WVP);
   end Set_WVP;

   --  -------------------------------------------------------------------------

   function Simple_Colour_Program  (theTechnique : Colour_Technique)
                                    return GL.Objects.Programs.Program is
   begin
      return theTechnique.Colour_Program;
   end Simple_Colour_Program;

   --  -------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Colour_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
      Shaders_List : GL.Objects.Shaders.Lists.List :=
                       GL.Objects.Programs.Attached_Shaders (theTechnique.Colour_Program);
      Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Colour_Program) then
         if Curs = GL.Objects.Shaders.Lists.No_Element then
            Put_Line ("Simple_Colour_Technique.Use_Program, Shaders list is empty");
         else
            GL.Objects.Programs.Use_Program (theTechnique.Colour_Program);
         end if;
      else
         Put_Line ("Simple_Colour_Technique.Use_Program Picking_Program link check failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Simple_Colour_Technique.Picking_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------

end Simple_Colour_Technique;
