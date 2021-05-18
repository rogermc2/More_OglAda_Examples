
package Levels_Manager is

    type Game_Status is (Game_Splash, Game_Loading, Game_Menu, Game_Credits,
                         Game_Running, Game_Next_Level, Game_Paused, Game_Over);
    Game_State  : Game_Status := Game_Splash;

    procedure Next_Level (Game_State : in out Game_Status;
                         Pickup_Spawn_Threshold : in out Float);

end Levels_Manager;
