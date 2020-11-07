
package Main_Game_Loop_Support is

   procedure Update_Timers (Main_Menu_Open                       : Boolean;
                            Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms, Level_Time : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count  : in out Integer);

end Main_Game_Loop_Support;
