
package Audio_Manager is

   Audio_Exception : Exception;

   type Sound is (Button_Sound, Jump_Sound, Movement_Sound, Oilcan_Sound, Water_Sound);

   procedure Close;
   procedure Init_Fmod;
   procedure Load_Audio;
   procedure Pause_Movement_Channel (Pause : Boolean);
   procedure Play_Sound (aSound : Sound);
   procedure Close_Audio;

end Audio_Manager;
