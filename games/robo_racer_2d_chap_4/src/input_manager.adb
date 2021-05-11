
with Glfw.Input.Keys;

package body Input_Manager is

    Current_Command : Command;

    --  ------------------------------------------------------------------------

    function Get_Command return Command is
    begin
        return Current_Command;
    end Get_Command;

    --  ------------------------------------------------------------------------

    procedure Update (Window : in out Input_Callback.Callback_Window;
                      Delta_Time : Float) is
        use Glfw.Input.Keys;
        use Input_Callback;
    begin
         if Was_Key_Pressed (Window, Left) or Was_Key_Pressed (Window, A) then
            Current_Command := Command_Left;
         elsif Was_Key_Pressed (Window, Right) or
          Was_Key_Pressed (Window, D) then
            Current_Command := Command_Right;
         elsif Was_Key_Pressed (Window, Up) then
            Current_Command := Command_Up;
         elsif Was_Key_Pressed (Window, Down) then
            Current_Command := Command_Down;
         elsif Was_Key_Pressed (Window, Escape) or
          Was_Key_Pressed (Window, Q) then
            Current_Command := Command_Up;
         else
            Current_Command := Command_Stop;
         end if;
    end Update;

    --  ------------------------------------------------------------------------

end Input_Manager;
