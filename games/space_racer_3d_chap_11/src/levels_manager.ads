
package Levels_Manager is

    type Game_Status is (Game_Splash, Game_Loading, Game_Menu, Game_Credits,
                         Game_Running, Game_Next_Level, Game_Paused,
                         Game_Restart, Game_Over, Game_Quit);
    function Get_Game_State return Game_Status;
    procedure Set_Game_State (State : Game_Status);

end Levels_Manager;