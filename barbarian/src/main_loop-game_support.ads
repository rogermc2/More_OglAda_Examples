
with GL.Types;

with Camera;

package Main_Loop.Game_Support is

   function Cheat_Check_1 return Boolean;
   procedure Check_Victory_Defeat;
   procedure Player_1_View;
   procedure Unload_Level;
   procedure Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count  : in out Integer);

end Main_Loop.Game_Support;
