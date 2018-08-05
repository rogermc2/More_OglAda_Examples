
with Ada.Text_IO; use Ada.Text_IO;

package body PS_Update_Technique is

   procedure Init (theTechnique : out Update_Technique;
                  Update_Program : GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      Varyings : constant String := "Type1, Position1, Velocity1, Age1";
   begin
      Transform_Feedback_Varyings (Update_Program, Varyings, Interleaved_Attribs);
      theTechnique.Delta_Millisec_Location :=
        Uniform_Location (Update_Program, "gDeltaTimeMillis");
      theTechnique.Random_Texture_Location :=
        Uniform_Location (Update_Program, "gRandomTexture");
      theTechnique.Time_Location :=
        Uniform_Location (Update_Program, "gTime");
      theTechnique.Launcher_Lifetime_Location :=
        Uniform_Location (Update_Program, "gLauncherLifetime");
      theTechnique.Shell_Lifetime_Location :=
        Uniform_Location (Update_Program, "gShellLifetime");
      theTechnique.Secondary_Shell_Lifetime_Location :=
        Uniform_Location (Update_Program, "gSecondaryShellLifetime");
   exception
      when  others =>
         Put_Line ("An exception occurred in PS_Update_Technique.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                                 Delta_Time : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (theTechnique.Delta_Millisec_Location, Delta_Time);
   end Set_Delta_Millisec;

   --  -------------------------------------------------------------------------

   procedure Set_Time (theTechnique : Update_Technique;
                       theTime : GL.Types.Int) is
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
