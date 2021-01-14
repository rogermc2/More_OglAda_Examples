
with GL.Types;

package Audio is

   Audio_Exception : Exception;

   function Create_Boulder_Sound (Pos : GL.Types.Singles.Vector3)
                                  return Positive;
   procedure Pause_Music (Pause : Boolean);
   procedure Play_Credits_Music (File_Name : String);
   procedure Play_Music (File_Name : String);
   procedure Play_Sound (File_Name : String; Random_Pitch : Boolean);
   procedure Set_Audio_Volume (Volume : Integer);
   function Start_Boulder_Sound (Index : Positive) return Boolean;
   procedure Stop_All_Boulder_Sounds;
   procedure Stop_All_Sounds;
   procedure Stop_Boulder_Sound (Index : Integer);
   procedure Stop_Credits_Music;
   procedure Update_Ambient_Sounds;
   procedure Update_Boulder_Sounds;

end Audio;
