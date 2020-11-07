
with Glfw;

with Event_Controller;
with FB_Effects;
with GUI_Level_Chooser;
with Manifold;
with Sprite_Renderer;
with Sprite_World_Map;
with Text;

package body Main_Game_Loop_Support is

   procedure Unload_Level (Camera_World_Pos : in out GL.Types.Singles.Vector3) is
      use GUI_Level_Chooser;
   begin
      Set_Cheated_On_Map (False);
      Set_Boulder_Crushes (0);
      Set_Hammer_Kills (0);
      Set_Fall_Kills (0);
      Camera_World_Pos := (2.0, 10.0, 2.0);
      FB_Effects.Set_Feedback_Effect (FB_Effects.FB_Default);
      Event_Controller.Reset;
      Text.Unload_Comic_Texts;
      Sprite_World_Map.Free_Sprite_World_Map;
      Sprite_Renderer.Clear_Sprites;

   end  Unload_Level;

   --  -------------------------------------------------------------------------

   procedure Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count : in out Integer) is
      Current_Time     : constant Float := Float (Glfw.Time);
      Delta_Time_Ms    : Float := 0.0;
   begin
      Delta_Time := Current_Time - Last_Time;
      Delta_Time_Ms := 1000.0 * Delta_Time;
      Last_Time := Current_Time;
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
