

with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Maths;
with Program_Loader;

Package body Shadow_Map_Technique is

   function Get_Display_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                          return GL.Uniforms.Uniform is
   begin
      return GL.Objects.Programs.Uniform_Location (Display_Program (theTechnique), Uniform_Name);
   end Get_Display_Uniform_Location;

   --  -------------------------------------------------------------------------

   function Get_Shadow_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                          return GL.Uniforms.Uniform is
   begin
      return GL.Objects.Programs.Uniform_Location (Shadow_Program (theTechnique), Uniform_Name);
   end Get_Shadow_Uniform_Location;

   --  -------------------------------------------------------------------------


   procedure Init (theTechnique : out Technique) is
      use Program_Loader;
      use  GL.Objects.Shaders;
   begin
      theTechnique.Display_Program := Program_From
        ((Src ("src/shaders/display_23.vs", Vertex_Shader),
         Src ("src/shaders/display_23.fs", Fragment_Shader)));
      theTechnique.Shadow_Program := Program_From
        ((Src ("src/shaders/shadow_map_23.vs", Vertex_Shader),
         Src ("src/shaders/shadow_map_23.fs", Fragment_Shader)));

      --        GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
      theTechnique.Display_WVP_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Display_Program, "gWVP");
      theTechnique.Display_Texture_Unit_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Display_Program, "gShadowMap");
      theTechnique.Shadow_WVP_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Shadow_Program, "gWVP");
      theTechnique.Shadow_Texture_Unit_Location := GL.Objects.Programs.Uniform_Location
        (theTechnique.Shadow_Program, "gShadowMap");
   end Init;

   --  -------------------------------------------------------------------------

   function Display_Program (theTechnique : Technique)
                           return GL.Objects.Programs.Program is
   begin
      return theTechnique.Display_Program;
   end Display_Program;

   --  -------------------------------------------------------------------------

   function Shadow_Program (theTechnique : Technique)
                           return GL.Objects.Programs.Program is
   begin
      return theTechnique.Shadow_Program;
   end Shadow_Program;

   --  -------------------------------------------------------------------------

   procedure Set_Shadow_Map_Texture_Units (theTechnique : Technique;
                                          Texture_Unit : GL.Types.Int) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Display_Program);
      GL.Uniforms.Set_Int (theTechnique.Display_Texture_Unit_Location, Texture_Unit);
      GL.Objects.Programs.Use_Program (theTechnique.Shadow_Program);
      GL.Uniforms.Set_Int (theTechnique.Shadow_Texture_Unit_Location, Texture_Unit);
   end Set_Shadow_Map_Texture_Units;

   --  -------------------------------------------------------------------------

   procedure Set_Display_WVP (theTechnique : Technique;
                             WVP          : GL.Types.Singles.Matrix4) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Display_Program);
      GL.Uniforms.Set_Single (theTechnique.Display_WVP_Location, WVP);
   end Set_Display_WVP;

   --  -------------------------------------------------------------------------

   procedure Set_Shadow_WVP (theTechnique : Technique;
                             WVP          : GL.Types.Singles.Matrix4) is
   begin
      GL.Objects.Programs.Use_Program (theTechnique.Shadow_Program);
      GL.Uniforms.Set_Single (theTechnique.Shadow_WVP_Location, WVP);
   end Set_Shadow_WVP;

   --  -------------------------------------------------------------------------

   procedure Use_Display_Program (theTechnique : Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Display_Program) then
         declare
            Shaders_List : GL.Objects.Shaders.Lists.List :=
                             GL.Objects.Programs.Attached_Shaders (theTechnique.Display_Program);
            Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
         begin
            if Curs = GL.Objects.Shaders.Lists.No_Element then
               Put_Line ("Shadow_Map_Technique.Use_Display_Program, Shaders list is empty");
            else
               GL.Objects.Programs.Use_Program (theTechnique.Display_Program);
            end if;
         end;  -- declare block
      else
         Put_Line ("Shadow_Map_Technique.Use_Display_Program, Display_Program link failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Shadow_Map_Technique.Use_Display_Program.");
         raise;
   end Use_Display_Program;

   --  -------------------------------------------------------------------------

   procedure Use_Shadow_Program (theTechnique : Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Shadow_Program) then
         declare
            Shaders_List : GL.Objects.Shaders.Lists.List :=
                             GL.Objects.Programs.Attached_Shaders (theTechnique.Shadow_Program);
            Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
         begin
            if Curs = GL.Objects.Shaders.Lists.No_Element then
               Put_Line ("Shadow_Map_Technique.Use_Shadow_Program, Shaders list is empty");
            else
               GL.Objects.Programs.Use_Program (theTechnique.Shadow_Program);
            end if;
         end;  -- declare block
      else
         Put_Line ("Shadow_Map_Technique.Use_Shadow_Program, Shadow_Program link failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Shadow_Map_Technique.Use_Shadow_Program.");
         raise;
   end Use_Shadow_Program;

   --  -------------------------------------------------------------------------

end Shadow_Map_Technique;
