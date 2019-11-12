

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

Package body Shadow_Map_Technique is

   procedure Init (theTechnique : out Technique) is
      use Program_Loader;
      use GL.Objects.Shaders;
   begin
      theTechnique.Shadow_Map_Program := Program_From
        ((Src ("src/shaders/shadow_map_24N.vs", Vertex_Shader),
          Src ("src/shaders/shadow_map_24N.fs", Fragment_Shader)));

      theTechnique.Shadow_WVP_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Shadow_Map_Program, "gWVP");
      theTechnique.Shadow_Texture_Unit_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Shadow_Map_Program, "gShadowMap");
   end Init;

   --  -------------------------------------------------------------------------

   function Shadow_Map_Program (theTechnique : Technique)
                                return GL.Objects.Programs.Program is
   begin
      return theTechnique.Shadow_Map_Program;
   end Shadow_Map_Program;

   --  -------------------------------------------------------------------------

   procedure Set_Texture_Unit (theTechnique : Technique;
                               Texture_Unit : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (theTechnique.Shadow_Texture_Unit_Location, Texture_Unit);
   end Set_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_WVP (theTechnique : Technique; WVP : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Shadow_WVP_Location, WVP);
   end Set_WVP;

   --  -------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
   begin
      if Link_Status (theTechnique.Shadow_Map_Program) then
         declare
            Shaders_List : constant GL.Objects.Shaders.Lists.List :=
                             GL.Objects.Programs.Attached_Shaders (theTechnique.Shadow_Map_Program);
            Curs         : constant GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
         begin
            if Curs = GL.Objects.Shaders.Lists.No_Element then
               Put_Line ("Shadow_Map_Technique.Use_Display_Program, Shaders list is empty");
            else
               GL.Objects.Programs.Use_Program (theTechnique.Shadow_Map_Program);
            end if;
         end;  -- declare block
      else
         Put_Line ("Shadow_Map_Technique.Use_Display_Program, Display_Program link failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Shadow_Map_Technique.Use_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------

end Shadow_Map_Technique;
