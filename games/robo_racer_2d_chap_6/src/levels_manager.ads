
with Input_Callback;

package Levels_Manager is

    type Game_Status is (Game_Splash, Game_Loading, Game_Menu, Game_Credits,
                         Game_Running, Game_Next_Level, Game_Paused, Game_Over);

    procedure Draw_Stats (Window   : in out Input_Callback.Callback_Window;
                          Pickups_Received, Enemies_Hit : Integer);
    function Get_Game_State return Game_Status;
    procedure Next_Level (Game_State : in out Game_Status;
                          Level_Timer, Pickup_Spawn_Threshold : in out Float;
                          Pickups_Received : Integer);
    procedure Set_Game_State (State : Game_Status);

end Levels_Manager;
