
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Objects.Shaders;
with GL.Objects.Shaders.Lists;

with Program_Loader;

package body Billboard_Technique_27 is 
    
   function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
    
   --  -------------------------------------------------------------------------
        
   function Get_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                   return GL.Uniforms.Uniform is
   begin
      return GL.Objects.Programs.Uniform_Location (Light_Program (theTechnique), Uniform_Name);
   end Get_Uniform_Location;

   --  -------------------------------------------------------------------------

   function Init (theTechnique  : out Technique) return Boolean is
      use GL.Objects.Shaders;
      use Program_Loader;
      OK  : Boolean := False;
   begin
      theTechnique.Lighting_Program := Program_From
        ((Src ("src/shaders/billboard_27.vs", Vertex_Shader),
         Src ("src/shaders/billboard_27.fs", Fragment_Shader),
         Src ("src/shaders/billboard_27.gs", Fragment_Shader)));

      OK := GL.Objects.Programs.Link_Status (theTechnique.Lighting_Program);
      if not OK then
         Put_Line ("Billboard_Technique_27.Init Build_Shader_Program, theTechnique.Lighting_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Lighting_Program));
      else
         GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
         theTechnique.VP_Location :=
           GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gVP");
         theTechnique.Camera_Pos_Location :=
           GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gCameraPos");
         theTechnique.Colour_Map_Location :=
           GL.Objects.Programs.Uniform_Location (theTechnique.Lighting_Program, "gColorMap");             
          end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Billboard_Technique_27.Init.");
         raise;
   end Init;
   
   --   -------------------------------------------------------------------------
 
   function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program is
   begin
      return theTechnique.Lighting_Program;
   end Light_Program;

   --  -------------------------------------------------------------------------
  
   procedure Set_Camera_Position_Location (theTechnique  : Technique;
                                           Pos : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Camera_Pos_Location, Pos);
   end Set_Camera_Position_Location;
   
   --   ----------------------------------------------------------------------------

    procedure Set_Colour_Texture_Unit_Location (theTechnique : Technique; Texture_Unit : Int) is
   begin
      GL.Uniforms.Set_Int (theTechnique.Colour_Map_Location, Texture_Unit);
   end Set_Colour_Texture_Unit_Location;
   
   --   ----------------------------------------------------------------------------

   procedure Set_VP_Location (theTechnique : Technique; VP : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (theTechnique.VP_Location, VP);    
   end Set_VP_Location;
   
   --   ---------------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Technique) is
      use GL.Objects.Shaders.Lists;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Lighting_Program) then
         declare
            Shaders_List : constant GL.Objects.Shaders.Lists.List :=
                             GL.Objects.Programs.Attached_Shaders 
                               (theTechnique.Lighting_Program);
            Curs         : constant GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
         begin
            if Curs = GL.Objects.Shaders.Lists.No_Element then
               Put_Line ("Billboard_Technique_27.Use_Program, Shaders list is empty");
            else
               GL.Objects.Programs.Use_Program (theTechnique.Lighting_Program);
            end if;
         end;  -- declare block
      else
         Put_Line ("Billboard_Technique_27.Use_Program, Lighting_Program link check failed");      
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Billboard_Technique_27.Use_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------
    
end Billboard_Technique_27;
