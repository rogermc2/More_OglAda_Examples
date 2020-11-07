
with GL.Types;

package Main_Game_Loop_Support is

   procedure Unload_Level (Camera_World_Pos : in out GL.Types.Singles.Vector3);
   procedure Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count  : in out Integer);

end Main_Game_Loop_Support;
