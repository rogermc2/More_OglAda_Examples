
package Audio_Manager is

   Audio_Exception : Exception;

   procedure Close;
   procedure Init_Fmod;
   procedure Load_Audio;
   procedure Pause_Movement_Channel (Pause : Boolean);

end Audio_Manager;
