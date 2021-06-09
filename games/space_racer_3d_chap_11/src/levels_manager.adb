
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Types;

with Text_Manager;

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
