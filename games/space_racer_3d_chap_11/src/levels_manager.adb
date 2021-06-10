
package body Levels_Manager is

   Game_State              : Game_Status := Game_Splash;

   --  ------------------------------------------------------------------------

    function Get_Game_State return Game_Status is
    begin
        return  Game_State;
    end Get_Game_State;

   --  ------------------------------------------------------------------------

    procedure Set_Game_State (State : Game_Status) is
    begin
        Game_State := State;
    end Set_Game_State;

    --  ------------------------------------------------------------------------

end Levels_Manager;
