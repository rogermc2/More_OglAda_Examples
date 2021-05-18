
with Glfw;

with GL.Types;

with Player_Manager;
with Text_Manager;

package body Levels_Manager is

    Pickups_Threshold       : constant Integer := 5;
    Pickup_Spawn_Adjustment : constant Float := 0.25;

   --  -------------------------------------------------------------------------------------------------

   procedure Game_Stats (Window   : in out Input_Callback.Callback_Window;
                         Pickups_Received, Enemies_Hit : Integer) is
      use GL.Types;
      use Player_Manager;
      Pickups_Stat  : constant String := "Pickups: " & Integer'Image (Pickups_Received);
      Enemies_Stat  : constant String := "Enemies Hit:" & Integer'Image (Enemies_Hit);
      Score         : constant String :=
                        "Score: " & Integer'Image (Get_Value (Get_Current_Player));
      Screen_Width  : Glfw.Size;
      Screen_Height : Glfw.Size;
      Height        : Single;
    begin
      Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      Height := Single (Screen_Height);
      Text_Manager.Draw_Text (Window, Enemies_Stat, 350.0, Height - 270.0, 0.0, 0.0, 1.0);
      Text_Manager.Draw_Text (Window, Pickups_Stat, 350.0, Height - 320.0, 0.0, 0.0, 1.0);
      Text_Manager.Draw_Text (Window, Score, 350.0, Height - 370.0, 0.0, 0.0, 1.0);
   end Game_Stats;

   --  -------------------------------------------------------------------------------------------------

    procedure Next_Level (Game_State : in out Game_Status;
                          Level_Timer, Pickup_Spawn_Threshold : in out Float;
                          Pickups_Received : Integer) is
    begin
        if Pickups_Received < Pickups_Threshold then
            Game_State := Game_Over;
        else
            Pickup_Spawn_Threshold := Pickup_Spawn_Threshold +
              Pickup_Spawn_Adjustment;
            Level_Timer := 0.0;
            Game_State := Game_Next_Level;
        end if;
   end Next_Level;

   --  -------------------------------------------------------------------------------------------------

end Levels_Manager;
