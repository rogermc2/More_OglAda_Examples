
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders.Lists;

with Program_Loader;
with Utilities;

with OglDev_Technique;

package body PS_Update_Technique is

   function Get_Random_Texture_Location (theTechnique : Update_Technique)
                                         return GL.Uniforms.Uniform is
   begin
      return theTechnique.Random_Texture_Location;
   end Get_Random_Texture_Location;

   --  -------------------------------------------------------------------------

   function Get_Time_Location (theTechnique : Update_Technique)
                               return GL.Uniforms.Uniform is
   begin
      return theTechnique.Time_Location;
   end Get_Time_Location;

   --  -------------------------------------------------------------------------

   function Get_Update_Program (theTechnique : Update_Technique)
                                return GL.Objects.Programs.Program is
   begin
      return theTechnique.Update_Program;
   end Get_Update_Program;

   --  -------------------------------------------------------------------------

   procedure Init (theTechnique : in out Update_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      Varyings       : constant String := "Type1, Position1, Velocity1, Age1";
      OK             : Boolean;
   begin
      theTechnique.Update_Program := Program_From
        ((Src ("src/shaders/ps_update.vs", Vertex_Shader),
         Src ("src/shaders/ps_update.gs", Geometry_Shader),
         Src ("src/shaders/ps_update.fs", Fragment_Shader)));
      --  Program_From includes linking

--         If not OglDev_Technique.Finalize (theTechnique.Update_Program) then
--              raise Update_Technique_Exception with "PS_Update_Technique.Init, Finalize failed";
--         end if;

      OK := GL.Objects.Programs.Link_Status (theTechnique.Update_Program);
      if not OK then
         Put_Line ("PS_Update_Technique.Init, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Update_Program));
      else
         Put_Line ("PS_Update_Technique.Init, Update_Program Link ok");
      end if;

      Use_Program (theTechnique.Update_Program);
      Transform_Feedback_Varyings (theTechnique.Update_Program, Varyings, Interleaved_Attribs);

      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gDeltaTimeMillis",
                                      theTechnique.Delta_Millisec_Location);
      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gRandomTexture",
                                      theTechnique.Random_Texture_Location);
      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gTime",
                                      theTechnique.Time_Location);
      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gLauncherLifetime",
                                      theTechnique.Launcher_Lifetime_Location);
      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gShellLifetime",
                                      theTechnique.Shell_Lifetime_Location);
      Utilities.Set_Uniform_Location (theTechnique.Update_Program, "gSecondaryShellLifetime",
                                      theTechnique.Secondary_Shell_Lifetime_Location);
   exception
      when  others =>
         Put_Line ("An exception occurred in PS_Update_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                                 Delta_Time   : GL.Types.Int) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Single (theTechnique.Delta_Millisec_Location,  GL.Types.Single (Delta_Time));
   end Set_Delta_Millisec;

   --  -------------------------------------------------------------------------

   procedure Set_Time (theTechnique : Update_Technique;
                       theTime      : GL.Types.Int) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Single (theTechnique.Time_Location, GL.Types.Single (theTime));
   end Set_Time;

   --  -------------------------------------------------------------------------

   procedure Set_Random_Texture_Unit (theTechnique : Update_Technique;
                                      Texture_Unit : GL.Types.Int) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Int (theTechnique.Random_Texture_Location, Texture_Unit);
   end Set_Random_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Launcher_Lifetime (theTechnique : Update_Technique;
                                    Lifetime     : GL.Types.Single) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Single (theTechnique.Launcher_Lifetime_Location, Lifetime);
   end Set_Launcher_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Shell_Lifetime (theTechnique : Update_Technique;
                                 Lifetime     : GL.Types.Single) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Single (theTechnique.Shell_Lifetime_Location, Lifetime);
   end Set_Shell_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Secondary_Shell_Lifetime (theTechnique : Update_Technique;
                                           Lifetime     : GL.Types.Single) is
   begin
      Use_Program (theTechnique);
      GL.Uniforms.Set_Single (theTechnique.Secondary_Shell_Lifetime_Location, Lifetime);
   end Set_Secondary_Shell_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Update_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
   begin
              if not GL.Objects.Programs.Validate_Status (theTechnique.Update_Program) then
      --              Put_Line ("PS_Update_Technique.Use_Program Update_Program validation failed.");
      --          else
      --              Put_Line ("PS_Update_Technique.Use_Program Update_Program validated.");
      declare
         Shaders_List : GL.Objects.Shaders.Lists.List :=
                          GL.Objects.Programs.Attached_Shaders (theTechnique.Update_Program);
         Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
      begin
         if Curs = GL.Objects.Shaders.Lists.No_Element then
            Put_Line ("PS_Update_Technique.Use_Program, Shaders list is empty");
         else
            GL.Objects.Programs.Use_Program (theTechnique.Update_Program);
         end if;
      end;  -- declare block
              end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in PS_Update_Technique.Use_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------
end PS_Update_Technique;
