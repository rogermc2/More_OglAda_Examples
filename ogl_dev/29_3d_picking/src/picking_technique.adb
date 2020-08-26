
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders.Lists;

with Program_Loader;
with Utilities;

with OglDev_Technique;

package body Picking_Technique is

   function Active_Attributes (theTechnique : Update_Technique) return GL.Types.Size is
   begin
      return GL.Objects.Programs.Active_Attributes (theTechnique.Update_Program);
   end Active_Attributes;

   --  -------------------------------------------------------------------------

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
      Varyings  : constant String := "Type1,Position1,Velocity1,Age1";
      OK        : Boolean;
   begin
      --  Program_From includes linking
      theTechnique.Update_Program := Program_From
        ((Src ("src/shaders/ps_update.vs", Vertex_Shader),
         Src ("src/shaders/ps_update.gs", Geometry_Shader)));

      OK := GL.Objects.Programs.Link_Status (theTechnique.Update_Program);
      if not OK then
         Put_Line ("Picking_Technique.Init, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Update_Program));
      end if;

      Use_Program (theTechnique.Update_Program);
      --  Interleaved_Attribs means that the varyings are recorded
      --  consecetively into a single buffer.
      Transform_Feedback_Varyings (theTechnique.Update_Program, Varyings,
                                   Interleaved_Attribs);
      theTechnique.Update_Program.Link;
      OK := GL.Objects.Programs.Link_Status (theTechnique.Update_Program);
      if not OK then
         Put_Line ("Picking_Technique.Init, Update_Program Link for Varyings failed");
         Put_Line (GL.Objects.Programs.Info_Log (theTechnique.Update_Program));
      end if;

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
         Put_Line ("An exception occurred in Picking_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                                 Delta_Time   : GL.Types.UInt) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Delta_Millisec_Location, GL.Types.Single (Delta_Time));
   end Set_Delta_Millisec;

   --  -------------------------------------------------------------------------

   procedure Set_Time (theTechnique : Update_Technique;
                       theTime      : GL.Types.UInt) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Time_Location, GL.Types.Single (theTime));
   end Set_Time;

   --  -------------------------------------------------------------------------

   procedure Set_Random_Texture_Unit (theTechnique : Update_Technique;
                                      Texture_Unit : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (theTechnique.Random_Texture_Location, Texture_Unit);
   end Set_Random_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Launcher_Lifetime (theTechnique : Update_Technique;
                                    Lifetime     : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Launcher_Lifetime_Location, Lifetime);
   end Set_Launcher_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Shell_Lifetime (theTechnique : Update_Technique;
                                 Lifetime     : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Shell_Lifetime_Location, Lifetime);
   end Set_Shell_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Secondary_Shell_Lifetime (theTechnique : Update_Technique;
                                           Lifetime     : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Secondary_Shell_Lifetime_Location, Lifetime);
   end Set_Secondary_Shell_Lifetime;

   --  -------------------------------------------------------------------------

   function Update_Program  (theTechnique : Update_Technique)
                             return GL.Objects.Programs.Program is
   begin
      return theTechnique.Update_Program;
   end Update_Program;

   --  -------------------------------------------------------------------------

   procedure Use_Program (theTechnique : Update_Technique) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders.Lists;
      Shaders_List : GL.Objects.Shaders.Lists.List :=
                       GL.Objects.Programs.Attached_Shaders (theTechnique.Update_Program);
      Curs         : GL.Objects.Shaders.Lists.Cursor := Shaders_List.First;
   begin
      if GL.Objects.Programs.Link_Status (theTechnique.Update_Program) then
         if Curs = GL.Objects.Shaders.Lists.No_Element then
            Put_Line ("Picking_Technique.Use_Program, Shaders list is empty");
         else
            GL.Objects.Programs.Use_Program (theTechnique.Update_Program);
         end if;
      else
         Put_Line ("Picking_Technique.Use_Program Update_Program link check failed.");
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Technique.Use_Program.");
         raise;
   end Use_Program;

   --  -------------------------------------------------------------------------

end Picking_Technique;
