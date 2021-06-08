
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Types;

with Text_Manager;

package body Levels_Manager is

    Game_State              : Game_Status := Game_Splash;

    --  ------------------------------------------------------------------------

    procedure Draw_Stats (Window   : in out Input_Callback.Callback_Window) is
        use GL.Types;
--          Pickups_Stat  : constant String :=
--                            "Pickups: " & Integer'Image (Pickups_Received);
        Score         : constant String
          := "Score: " & Integer'Image (0);
        Screen_Width  : Glfw.Size;
        Screen_Height : Glfw.Size;
        Height        : Single;
    begin
        Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Height := Single (Screen_Height);
        Text_Manager.Draw_Text (Window, Score, 350.0, Height - 320.0,
                                0.0, 0.0, 1.0);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Text_Manager.Draw_Game_Stats.");
            Put_Line (Exception_Information (anError));
            raise;
    end Draw_Stats;

    --  -------------------------------------------------------------------------------------------------

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
