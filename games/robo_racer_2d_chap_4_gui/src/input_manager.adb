
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Mouse;
with Glfw.Input.Keys;

package body Input_Manager is

    use Sprite_Manager;
    package UI_Package is new
      Ada.Containers.Vectors (Positive, Sprite_Manager.Sprite);
    subtype UI_List is UI_Package.Vector;

    Current_Command : Command := Command_Stop;
    UI_Elements     : UI_List;

    --  ------------------------------------------------------------------------

    procedure Add_UI_Element (Element : Sprite_Manager.Sprite) is
    begin
        UI_Elements.Append (Element);
    end Add_UI_Element;

    --  ------------------------------------------------------------------------

    function Check_For_Click (Window : in out Input_Callback.Callback_Window;
                              UI_Element : Sprite) return Boolean is
        use Glfw.Input;
        Window_Width   : Glfw.Size;
        Window_Height  : Glfw.Size;
        Cursor_X       : Float;  --  Mouse.Coordinate;
        Cursor_Y       : Float;  -- Mouse.Coordinate;
        Left           : constant Float := Get_X (UI_Element);
        Bottom         : constant Float := Get_Y (UI_Element);
        Result         : Boolean := False;
    begin
        if Input_Callback.Is_Button_Down (Glfw.Input.Mouse.Left_Button) then
            Window'Access.Get_Size (Window_Width, Window_Height);
            Window'Access.Get_Cursor_Pos (Mouse.Coordinate (Cursor_X),
                                          Mouse.Coordinate (Cursor_Y));
            Cursor_Y := Float (Window_Height) - Cursor_Y;
            Result := Cursor_X >= Left and
              Cursor_X <= Left + Get_Width (UI_Element) and
              Cursor_Y >= Bottom and
              Cursor_Y <=  Bottom + Get_Height (UI_Element);
            --              Put_Line ("Input_Manager.Check_For_Click Left, Right, Bottom, Top: " &
            --                          Float'Image (Left) & ", " &
            --                          Float'Image (Left + Get_Width (UI_Element)) &
            --                          Float'Image (Bottom) & ", " &
            --                          Float'Image (Bottom + Get_Height (UI_Element)));
            --              Put_Line ("Input_Manager.Check_For_Click Cursor_X, Cursor_Y: " &
            --                          Float'Image (Cursor_X) & ", " &
            --                          Float'Image (Cursor_Y));
            --              Put_Line ("Input_Manager.Check_For_Click : Result " &
            --                          Boolean'Image (Result));
        end if;
        return Result;
    end Check_For_Click;

    --  ------------------------------------------------------------------------

    function Get_Current_Command return Command is
    begin
        return Current_Command;
    end Get_Current_Command;

    --  ------------------------------------------------------------------------

    procedure Set_Command_Invalid is
    begin
        Current_Command := Command_Invalid;
    end Set_Command_Invalid;

    --  ------------------------------------------------------------------------

    procedure Update_Command (Window : in out Input_Callback.Callback_Window) is
        use UI_Package;
        use Glfw.Input.Keys;
        use Input_Callback;

        procedure Check_Button_Click (Curs : Cursor) is
            Index      : constant Positive := To_Index (Curs);
            UI_Element : Sprite := UI_Elements.Element (Index);
        begin
            if Is_Active (UI_Element) then
                if Check_For_Click (Window, UI_Element) then
                    Put_Line ("Input_Manager.Check_Button_Click UI_Element.Position " &
                    Float'Image (Get_X (UI_Element)) &
                    Float'Image (Get_Y (UI_Element)));
                    Set_Clicked (UI_Element, True);
                    Put_Line ("Input_Manager.Check_Button_Click UI_Element clicked " &
                    Boolean'Image (Is_Clicked (UI_Element)));
                    Current_Command := Command_UI;
                end if;
            end if;
        end Check_Button_Click;

    begin
        UI_Elements.Iterate (Check_Button_Click'Access);

        if Current_Command /= Command_UI then
            if Is_Key_Down (Left) or Is_Key_Down (A) then
                Current_Command := Command_Left;
            elsif Is_Key_Down (Right) or Is_Key_Down (D) then
                Current_Command := Command_Right;
            elsif Is_Key_Down (Up) then
                Current_Command := Command_Up;
            elsif Is_Key_Down (Down) then
                Current_Command := Command_Down;
            elsif Current_Command /= Command_UI then
                Current_Command := Command_Stop;
            end if;
        end if;
    end Update_Command;

    --  ------------------------------------------------------------------------

end Input_Manager;
