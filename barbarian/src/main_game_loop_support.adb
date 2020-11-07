
with Glfw;

package body Main_Game_Loop_Support is

   procedure Update_Timers (Main_Menu_Open : Boolean;
                            Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms, Level_Time : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count : in out Integer) is
      Current_Time     : constant Float := Float (Glfw.Time);
      Delta_Time_Ms    : Float := 0.0;
   begin
      Delta_Time := Current_Time - Last_Time;
      Delta_Time_Ms := 1000.0 * Delta_Time;
      Last_Time := Current_Time;
      if not Main_Menu_Open then
         Level_Time := Level_Time + Delta_Time;
      end if;
      Avg_Frame_Time_Accum_Ms := Avg_Frame_Time_Accum_Ms + Delta_Time_Ms;
      Curr_Frame_Time_Accum_Ms := Curr_Frame_Time_Accum_Ms + Delta_Time_Ms;
      Avg_Frames_Count := Avg_Frames_Count + 1;
      Curr_Frames_Count := Curr_Frames_Count + 1;
      if Avg_Frames_Count > 999 then
         Avg_Frames_Count := 0;
         Avg_Frame_Time_Accum_Ms := 0.0;
      end if;
      if Curr_Frames_Count > 99 then
         Curr_Frames_Count := 0;
         Curr_Frame_Time_Accum_Ms := 0.0;
      end if;

   end Update_Timers;

end Main_Game_Loop_Support;
