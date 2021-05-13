
with Ada.Containers.Vectors;
--  with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Mouse;
with Glfw.Input.Keys;

with Sprite_Manager;

package body Input_Manager is

    use Sprite_Manager;
    package UI_Package is new
      Ada.Containers.Vectors (Positive, Sprite_Manager.Sprite);
    subtype UI_List is UI_Package.Vector;

    Current_Command : Command := Command_Stop;
    UI_Elements     : UI_List;

    --  ------------------------------------------------------------------------

    function Get_Command return Command is
    begin
        return Current_Command;
    end Get_Command;

    --  ------------------------------------------------------------------------

    procedure Add_UI_Element (Element : Sprite) is
    begin
        UI_Elements.Append (Element);
    end Add_UI_Element;

    --  ------------------------------------------------------------------------

    function Check_For_Click (Window : in out Input_Callback.Callback_Window;
                              UI_Element : Sprite) return Boolean is
        use Glfw.Input;
        Cursor_X  : Mouse.Coordinate;
        Cursor_Y  : Mouse.Coordinate;
        Left      : constant Float := Get_X (UI_Element);
        Bottom    : constant Float := Get_Y (UI_Element);
        Result    : Boolean := False;
    begin
        if Input_Callback.Is_Button_Down (Glfw.Input.Mouse.Left_Button) then
            Window'Access.Get_Cursor_Pos (Mouse.Coordinate (Cursor_X),
                                          Mouse.Coordinate (Cursor_Y));
            Result := Float (Cursor_X) >= Left and
              Float (Cursor_X) <= Left + Get_Width (UI_Element) and
              Float (Cursor_Y) >= Bottom and
              Float (Cursor_Y) <=  Bottom + Get_Height (UI_Element);
        end if;
        return Result;
    end Check_For_Click;

    --  ------------------------------------------------------------------------

    procedure Update_Command (Window : in out Input_Callback.Callback_Window) is
        use UI_Package;
        use Glfw.Input.Keys;
        use Input_Callback;
        procedure Check_Click (Curs : Cursor) is
            Index      : constant Positive := To_Index (Curs);
            UI_Element : Sprite := UI_Elements.Element (Index);
        begin
            if Is_Active (UI_Element) then
                if Check_For_Click (Window, UI_Element) then
                    Set_Clicked (UI_Element, True);
                    Current_Command := Command_UI;
                end if;
            end if;
        end Check_Click;
    begin
        UI_Elements.Iterate (Check_Click'Access);

        if Is_Key_Down (Left) or Is_Key_Down (A) then
            Current_Command := Command_Left;
        elsif Is_Key_Down (Right) or Is_Key_Down (D) then
            Current_Command := Command_Right;
        elsif Is_Key_Down (Up) then
            Current_Command := Command_Up;
        elsif Is_Key_Down (Down) then
            Current_Command := Command_Down;
        else
            Current_Command := Command_Stop;
        end if;
    end Update_Command;

    --  ------------------------------------------------------------------------

end Input_Manager;
