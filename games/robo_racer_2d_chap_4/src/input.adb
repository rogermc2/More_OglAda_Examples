
with Glfw.Input.Keys;

package body Input is

    aCommand : Command;

    --  ------------------------------------------------------------------------

    function Get_Command return Command is
    begin
        return aCommand;
    end Get_Command;

    --  ------------------------------------------------------------------------

    procedure Update (Window : in out Input_Callback.Call_Back_Window;
                      Delta_Time : Float) is
        use Glfw.Input.Keys;
        use Input_Callback;
    begin
         if Was_Key_Pressed (Window, Escape) then
            null;
         end if;
    end Update;

    --  ------------------------------------------------------------------------

end Input;
