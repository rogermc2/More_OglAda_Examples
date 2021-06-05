
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Mouse;
with Glfw.Input.Keys;

with Sprite_Manager;

package body Input_Manager is

   use Sprite_Manager;
   UI_Elements    : array (Button_Index range Button_Index'Range) of
     Sprite_Manager.Sprite;

   Current_Command : Command := Command_None;

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

   procedure Load_Buttons (Screen : in out Input_Callback.Callback_Window) is
        B_Height      : constant Float := 75.0;
        B_Width       : constant Float := 38.0;
        Screen_Width  : Glfw.Size;
        Screen_Height : Glfw.Size;
        S_Height      : Float;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      S_Height := Float (Screen_Height);

      Set_Frame_Size (UI_Elements (Pause_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Pause_Button), 1);
      Set_Position (UI_Elements (Pause_Button), 10.0, 10.0);
      Add_Texture (UI_Elements
                   (Pause_Button), "src/resources/pauseButton.png", False);
      Set_Visible (UI_Elements (Pause_Button), True);
      Set_Active (UI_Elements (Pause_Button), True);

      Set_Frame_Size (UI_Elements (Resume_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Resume_Button), 1);
      Set_Position (UI_Elements (Resume_Button), 80.0, 10.0);
      Add_Texture (UI_Elements
                   (Resume_Button), "src/resources/resumeButton.png", False);

      Set_Frame_Size (UI_Elements (Play_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Play_Button), 1);
      Set_Position (UI_Elements (Play_Button), 390.0, S_Height - 350.0);
      Set_Visible (UI_Elements (Play_Button), True);
      Add_Texture (UI_Elements
                   (Play_Button), "src/resources/playButton.png", False);

      Set_Frame_Size (UI_Elements (Credits_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Credits_Button), 1);
      Set_Position (UI_Elements (Credits_Button), 390.0, S_Height - 400.0);
      Set_Visible (UI_Elements (Credits_Button), True);
      Add_Texture (UI_Elements
                   (Credits_Button), "src/resources/creditsButton.png", False);

      Set_Frame_Size (UI_Elements (Exit_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Exit_Button), 1);
      Set_Position (UI_Elements (Exit_Button), 390.0, 40.0);
      Set_Visible (UI_Elements (Exit_Button), True);
      Add_Texture (UI_Elements
                   (Exit_Button), "src/resources/exitButton.png", False);

      Set_Frame_Size (UI_Elements (Menu_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Menu_Button), 1);
      Set_Position (UI_Elements (Menu_Button), 390.0, S_Height - 450.0);
      Set_Visible (UI_Elements (Menu_Button), True);
      Add_Texture (UI_Elements
                   (Menu_Button), "src/resources/menuButton.png", False);

      Set_Frame_Size (UI_Elements (Continue_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Continue_Button), 1);
      Set_Position (UI_Elements (Continue_Button), 390.0, S_Height - 400.0);
      Set_Visible (UI_Elements (Continue_Button), True);
      Add_Texture (UI_Elements
                   (Continue_Button), "src/resources/continueButton.png", False);


      Set_Frame_Size (UI_Elements (Replay_Button), B_Height, B_Width);
      Set_Number_Of_Frames (UI_Elements (Replay_Button), 1);
      Set_Position (UI_Elements (Replay_Button), 390.0, 90.0);
      Set_Visible (UI_Elements (Replay_Button), True);
      Add_Texture (UI_Elements
                   (Replay_Button), "src/resources/replayButton.png", False);

   end Load_Buttons;

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

   procedure Render_Button (Render_Program : GL.Objects.Programs.Program;
                            Button : Button_Index) is
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
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

   procedure Set_Command_None is
   begin
      Current_Command := Command_None;
   end Set_Command_None;

   --  ------------------------------------------------------------------------

   procedure Update (Delta_Time : Float) is
   begin
      for index in Input_Manager.Button_Index'Range loop
         Sprite_Manager.Update (UI_Elements (index), Delta_Time);
      end loop;
   end Update;

   --  ------------------------------------------------------------------------

   procedure Update_Button (Button : Button_Index; Delta_Time : Float) is
   begin
         Sprite_Manager.Update (UI_Elements (Button), Delta_Time);
   end Update_Button;

   --  ------------------------------------------------------------------------

   procedure Update_Command (Window : in out Input_Callback.Callback_Window) is
      use Glfw.Input.Keys;
      use Input_Callback;

      procedure Check_Button_Click (Index : Button_Index) is
      begin
         if Is_Active (UI_Elements (Index)) then
            if Check_For_Click (Window, UI_Elements (Index)) then
               Set_Clicked (UI_Elements (Index), True);
               Current_Command := Command_GUI;
            end if;
         end if;
      end Check_Button_Click;

   begin
      for index in Button_Index range Button_Index'Range loop
         Check_Button_Click (index);
      end loop;

      if Key_Pressed and then Current_Command /= Command_GUI then
         Put_Line ("Input_Manager.Update_Command Last Key " &
                     Key'Image (Last_Key_Down));
         if Is_Key_Down (Q) then
            Current_Command := Command_Quit;
         elsif Is_Key_Down (Left) or Is_Key_Down (A) then
            Current_Command := Command_Left;
         elsif Is_Key_Down (Right) or Is_Key_Down (D) then
            Current_Command := Command_Right;
         elsif Is_Key_Down (Up)  or Is_Key_Down (W) then
            Current_Command := Command_Up;
         elsif Is_Key_Down (Down) or Is_Key_Down (S) then
            Current_Command := Command_Down;
         elsif Is_Key_Down (Space) then
            Current_Command := Command_Stop;
         else
            Current_Command := Command_Invalid;
         end if;
      else
         Current_Command := Command_None;
      end if;

   end Update_Command;

   --  ------------------------------------------------------------------------

end Input_Manager;
