
with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package PS_Update_Technique is

   type Update_Technique is private;

   procedure Init (theTechnique : out Update_Technique;
                   Update_Program : GL.Objects.Programs.Program);
   procedure Set_Delta_Millisec (theTechnique : Update_Technique;
                             Delta_Time : GL.Types.Int);
   procedure Set_Time (theTechnique : Update_Technique;
                       theTime : GL.Types.Int);
   procedure Set_Random_Texture_Unit (theTechnique : Update_Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_Launcher_Lifetime (theTechnique : Update_Technique;
                                    Lifetime : GL.Types.Single);
   procedure Set_Shell_Lifetime (theTechnique : Update_Technique;
                                 Lifetime : GL.Types.Single);
   procedure Set_Secondary_Shell_Lifetime (theTechnique : Update_Technique;
                                           Lifetime : GL.Types.Single);

private
   type Update_Technique is record
      --  Shader_Object_List -- inherited from Technique.h
      Delta_Millisec_Location    : GL.Uniforms.Uniform;
      Random_Texture_Location    : GL.Uniforms.Uniform;
      Time_Location              : GL.Uniforms.Uniform;
      Launcher_Lifetime_Location : GL.Uniforms.Uniform;
      Shell_Lifetime_Location    : GL.Uniforms.Uniform;
      Secondary_Shell_Lifetime_Location  : GL.Uniforms.Uniform;
   end record;

end PS_Update_Technique;
