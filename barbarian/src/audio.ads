
package Audio is

   procedure Pause_Music (Pause : Boolean);
   procedure Play_Credits_Music (File_Name : String);
   procedure Play_Music (File_Name : String);
   procedure Play_Sound (File_Name : String; Random_Pitch : Boolean);
   procedure Set_Audio_Volume (Volume : Integer);
   procedure Stop_All_Sounds;
   procedure Stop_Credits_Music;

end Audio;
