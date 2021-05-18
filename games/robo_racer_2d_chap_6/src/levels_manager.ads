
with Input_Callback;

package Levels_Manager is

    type Game_Status is (Game_Splash, Game_Loading, Game_Menu, Game_Credits,
                         Game_Running, Game_Next_Level, Game_Paused, Game_Over);
    Game_State  : Game_Status := Game_Splash;

    procedure Game_Stats (Window   : in out Input_Callback.Callback_Window;
                          Pickups_Received, Enemies_Hit : Integer);
    procedure Next_Level (Game_State : in out Game_Status;
                          Level_Timer, Pickup_Spawn_Threshold : in out Float;
                          Pickups_Received : Integer);

end Levels_Manager;
