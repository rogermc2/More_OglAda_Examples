
with Glfw.Input.Mouse;
with Glfw.Input.Keys;

with Sprite_Manager;

package body Input_Manager is

   use Sprite_Manager;
   UI_Elements    : array (Button_Index range Button_Index'Range) of
     Sprite_Manager.Sprite;

   Current_Command : Command := Command_Stop;

   --  ------------------------------------------------------------------------

   function Check_For_Click (Window     : in out Input_Callback.Callback_Window;
                             UI_Element : Sprite) return Boolean is
      use Glfw.Input;
      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
      Cursor_X       : Float;  --  Mouse.Coordinate;
      Cursor_Y       : Float;  -- Mouse.Coordinate;
      Position       : constant Point := Get_Position (UI_Element);
      Left           : constant Float := Position.X;
      Bottom         : constant Float := Position.Y;
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
      end if;
      return Result;
   end Check_For_Click;

   --  ------------------------------------------------------------------------

   function Get_Current_Command return Command is
   begin
      return Current_Command;
   end Get_Current_Command;

   --  ------------------------------------------------------------------------

   procedure Init_Buttons is
   begin
      Set_Frame_Size (UI_Elements (Pause_Button), 75.0, 38.0);
      Set_Number_Of_Frames (UI_Elements (Pause_Button), 1);
      Set_Position (UI_Elements (Pause_Button), 10.0, 5.0);
      Add_Texture (UI_Elements (Pause_Button), "src/resources/pauseButton.png", False);
      Set_Visible (UI_Elements (Pause_Button), True);
      Set_Active (UI_Elements (Pause_Button), True);

      Set_Frame_Size (UI_Elements (Resume_Button), 75.0, 38.0);
      Set_Number_Of_Frames (UI_Elements (Resume_Button), 1);
      Set_Position (UI_Elements (Resume_Button), 80.0, 10.0);
      Add_Texture (UI_Elements (Resume_Button), "src/resources/resumeButton.png", False);

   end Init_Buttons;

   --  -------------------------------------------------------------------------

   function Is_Active (Button : Button_Index) return Boolean is
   begin
      return Sprite_Manager.Is_Active (UI_Elements (Button));
   end Is_Active;

   --  -------------------------------------------------------------------------

   function Is_Clicked (Button : Button_Index) return Boolean is
   begin
      return Sprite_Manager.Is_Clicked (UI_Elements (Button));
   end Is_Clicked;

   --  -------------------------------------------------------------------------

   function Is_Visible (Button : Button_Index) return Boolean is
   begin
      return Sprite_Manager.Is_Visible (UI_Elements (Button));
   end Is_Visible;

   --  -------------------------------------------------------------------------

   procedure Render_Button (Button : Button_Index) is
   begin
      Sprite_Manager.Render (UI_Elements (Button));
   end Render_Button;

   --  -------------------------------------------------------------------------

   procedure Set_Active (Button : Button_Index; State : Boolean) is
   begin
      Sprite_Manager.Set_Active (UI_Elements (Button), State);
   end Set_Active;

   --  -------------------------------------------------------------------------

   procedure Set_Visible (Button : Button_Index; State : Boolean) is
   begin
      Sprite_Manager.Set_Visible (UI_Elements (Button), State);
   end Set_Visible;

   --  -------------------------------------------------------------------------

   procedure Set_Clicked (Button : Button_Index; Clicked : Boolean) is
   begin
      Sprite_Manager.Set_Clicked (UI_Elements (Button), Clicked);
   end Set_Clicked;

   --  -------------------------------------------------------------------------

   procedure Set_Command_Invalid is
   begin
      Current_Command := Command_Invalid;
   end Set_Command_Invalid;

   --  ------------------------------------------------------------------------

   procedure Update (Button : Button_Index; Delta_Time : Float) is
   begin
      Sprite_Manager.Update (UI_Elements (Button), Delta_Time);
   end Update;

   --  ------------------------------------------------------------------------

   procedure Update_Command (Window : in out Input_Callback.Callback_Window) is
      use Glfw.Input.Keys;
      use Input_Callback;

      procedure Check_Button_Click (Index : Button_Index) is
      begin
         if Is_Active (UI_Elements (Index)) then
            if Check_For_Click (Window, UI_Elements (Index)) then
               Set_Clicked (UI_Elements (Index), True);
               Current_Command := Command_UI;
            end if;
         end if;
      end Check_Button_Click;

   begin
      for index in Button_Index range Button_Index'Range loop
         Check_Button_Click (index);
      end loop;

      if Current_Command /= Command_UI then
         if Is_Key_Down (Left) or Is_Key_Down (A) then
            Current_Command := Command_Left;
         elsif Is_Key_Down (Right) or Is_Key_Down (D) then
            Current_Command := Command_Right;
         elsif Is_Key_Down (Up) then
            Current_Command := Command_Up;
         elsif Is_Key_Down (Down) then
            Current_Command := Command_Down;
         elsif Is_Key_Down (S) then
            Current_Command := Command_Stop;
         end if;
      end if;
   end Update_Command;

   --  ------------------------------------------------------------------------

end Input_Manager;
