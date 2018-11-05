
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body PS_Update_Technique is

   procedure Init (theTechnique : in out Update_Technique;
                  Update_Program : GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      Varyings : constant String := "Type1, Position1, Velocity1, Age1";
   begin
      Use_Program (Update_Program);
      Put_Line ("PS_Update_Technique.Init using Update_Program.");
      Transform_Feedback_Varyings (Update_Program, Varyings, Interleaved_Attribs);
      Utilities.Set_Uniform_Location (Update_Program, "gDeltaTimeMillis",
                                      theTechnique.Delta_Millisec_Location);
      Utilities.Set_Uniform_Location (Update_Program, "gRandomTexture",
                                      theTechnique.Random_Texture_Location);
      Utilities.Set_Uniform_Location (Update_Program, "gTime",
                                      theTechnique.Time_Location);
      Utilities.Set_Uniform_Location (Update_Program, "gLauncherLifetime",
                                      theTechnique.Launcher_Lifetime_Location);
      Utilities.Set_Uniform_Location (Update_Program, "gShellLifetime",
                                      theTechnique.Shell_Lifetime_Location);
      Utilities.Set_Uniform_Location (Update_Program, "gSecondaryShellLifetime",
                                      theTechnique.Secondary_Shell_Lifetime_Location);
   exception
      when  others =>
         Put_Line ("An exception occurred in PS_Update_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                                 Delta_Time : GL.Types.Int) is
   begin
      Put_Line ("PS_Update_Technique.Set_Delta_Millisec theTechnique.Time_Location: " &
               GL.Uniforms.Uniform'Image (theTechnique.Time_Location));
      GL.Uniforms.Set_Int (theTechnique.Delta_Millisec_Location, Delta_Time);
   end Set_Delta_Millisec;

   --  -------------------------------------------------------------------------

   procedure Set_Time (theTechnique : Update_Technique;
                       theTime : GL.Types.Int) is
   begin
      Put_Line ("PS_Update_Technique.Set_Time theTechnique.Time_Location: " &
               GL.Uniforms.Uniform'Image (theTechnique.Time_Location));
      GL.Uniforms.Set_Single (theTechnique.Time_Location, GL.Types.Single (theTime));
      Put_Line ("PS_Update_Technique.Set_Time theTechnique.Time_Location set;");
   end Set_Time;

   --  -------------------------------------------------------------------------

   procedure Set_Random_Texture_Unit (theTechnique : Update_Technique;
                                      Texture_Unit : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (theTechnique.Random_Texture_Location, Texture_Unit);
   end Set_Random_Texture_Unit;

   --  -------------------------------------------------------------------------

   procedure Set_Launcher_Lifetime (theTechnique : Update_Technique;
                                    Lifetime : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Launcher_Lifetime_Location, Lifetime);
   end Set_Launcher_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Shell_Lifetime (theTechnique : Update_Technique;
                                 Lifetime : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Shell_Lifetime_Location, Lifetime);
   end Set_Shell_Lifetime;

   --  -------------------------------------------------------------------------

   procedure Set_Secondary_Shell_Lifetime (theTechnique : Update_Technique;
                                           Lifetime : GL.Types.Single) is
   begin
      GL.Uniforms.Set_Single (theTechnique.Secondary_Shell_Lifetime_Location, Lifetime);
   end Set_Secondary_Shell_Lifetime;

   --  -------------------------------------------------------------------------

end PS_Update_Technique;
