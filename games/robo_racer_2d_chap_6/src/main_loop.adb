
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Maths;
with Program_Loader;
with Utilities;

with Input_Manager;
with Player_Manager;
with Sprite_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is
   type Game_Status is (Game_Splash, Game_Loading, Game_Menu, Game_Credits,
                        Game_Running, Game_Next_Level, Game_Paused, Game_Over);

   Back                         : constant GL.Types.Colors.Color :=
                                    (0.6, 0.6, 0.6, 1.0);
   Border_Width                 : constant GL.Types.Size := 2;
   Splash_Threshold             : constant Float := 2.5;
   UI_Threshold                 : constant float := 0.1;
   Enemy_Spawn_Threshold        : constant Float := 3.5;
   Pickup_Spawn_Threshold       : constant Float := 2.5;
   Last_Time                    : Float := 0.0;
   Game_Program                 : GL.Objects.Programs.Program;
   Model_Uniform                : GL.Uniforms.Uniform;
   Projection_Uniform           : GL.Uniforms.Uniform;
   Texture_Uniform              : GL.Uniforms.Uniform;
   Background                   : Sprite_Manager.Sprite;
   Splash_Screen                : Sprite_Manager.Sprite;
   Menu_Screen                  : Sprite_Manager.Sprite;
   Credits_Screen               : Sprite_Manager.Sprite;
   Enemy                        : Sprite_Manager.Sprite;
   Pickup                       : Sprite_Manager.Sprite;
   Game_State                   : Game_Status := Game_Splash;
   UI_Timer                     : Float := 0.0;
   Splash_Timer                 : Float := 0.0;
   Pickup_Spawn_Timer           : Float := 0.0;
   Enemy_Spawn_Timer            : Float := 0.0;

   procedure Resize_GL_Scene  (Screen : in out Input_Callback.Callback_Window);

   --  ------------------------------------------------------------------------

   procedure Check_Background
     (Window : in out Input_Callback.Callback_Window) is
      use Sprite_Manager;
      Position        : constant Point := Get_Position (Background);
      Right_Threshold : Float;
      Screen_Width    : Glfw.Size;
      Screen_Height   : Glfw.Size;
   begin

      Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      Right_Threshold := Float (Screen_Width) - Get_Width (Background);

      if Position.X > 0.0 then
         Set_Position (Background, 0.0, Position.Y);
      elsif Position.X < Right_Threshold then
         Set_Position (Background, Right_Threshold, Position.Y);
      end if;

   end Check_Background;

   ----------------------------------------------------------------------------

   procedure Check_Boundaries
     (Window : in out Input_Callback.Callback_Window;
      Player :  Player_Manager.Player_Index) is
      use Sprite_Manager;
      use Player_Manager;
      Check         : constant Rectangle := Get_Collision_Rectangle (Player);
      Position      : constant Point := Get_Position (Player);
      Screen_Width  : Glfw.Size;
      Screen_Height : Glfw.Size;
      Offset        : Float;
      Pos_Offset    : Point;
   begin
      Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      if (Player = Robot_Left or Player = Robot_Left_Strip) and
        Check.Left <= 0.0 then
         Offset := Check.Left;
         Pos_Offset.X := Position.X - Offset;
         Pos_Offset.Y := Position.Y;
         Set_Position (Player, Pos_Offset);
         Set_Velocity (Player, 0.0);
      elsif
        (Player = Robot_Right or Player = Robot_Right_Strip) and
        Check.Right >= Float (Screen_Width) then
         Offset := Float (Screen_Width) - Check.Right;
         Pos_Offset.X := Position.X + Offset;
         Pos_Offset.Y := Position.Y;
         Set_Position (Player, Pos_Offset);
         Set_Velocity (Player, 0.0);
      elsif Check.Bottom < 0.0 then
         Offset := Check.Bottom;
         Pos_Offset.X := Position.X;
         Pos_Offset.Y := Position.Y - Offset;
         Set_Position (Player, Pos_Offset);
         Set_Velocity (Player, 0.0);
      elsif Check.Top > Float (Screen_Height) then
         Offset := Check.Top;
         Pos_Offset.X := Position.X;
         Pos_Offset.Y := Position.Y + Offset;
         Set_Position (Player, Pos_Offset);
         Set_Velocity (Player, 0.0);
      end if;
   end Check_Boundaries;

   ----------------------------------------------------------------------------

   procedure Draw_Score is
   begin
      null;
   end Draw_Score;

   ----------------------------------------------------------------------------


   procedure Check_Collisions is
      use Sprite_Manager;
      use Player_Manager;
      Index   : constant Player_Index := Get_Current_Player;
      Player  : Sprite := Get_Player (Index);
   begin

      if Intersect_Circle (Player, Pickup) then
         Set_Active (Pickup, False);
         Set_Visible (Pickup, False);
         Set_Value (Player,
                    Get_Value (Player) + Sprite_Manager.Get_Value (Pickup));
         Pickup_Spawn_Timer := 0.0;
      end if;

      if Intersect_Rectangle (Player, Enemy) then
         Set_Active (Enemy, False);
         Set_Visible (Enemy, False);
         Set_Value (Player,
                    Get_Value (Player) + Sprite_Manager.Get_Value (Enemy));
         Enemy_Spawn_Timer := 0.0;
      end if;

   end Check_Collisions;

   ----------------------------------------------------------------------------

   procedure Enable_Mouse_Callbacks
     (Window : in out Input_Callback.Callback_Window;
      Enable : Boolean) is
      use Glfw.Windows.Callbacks;
   begin
      if Enable then
         Window.Enable_Callback (Mouse_Position);
         Window.Enable_Callback (Mouse_Enter);
         Window.Enable_Callback (Mouse_Button);
         Window.Enable_Callback (Mouse_Scroll);
      else
         Window.Disable_Callback (Mouse_Position);
         Window.Disable_Callback (Mouse_Enter);
         Window.Disable_Callback (Mouse_Button);
         Window.Disable_Callback (Mouse_Scroll);
      end if;
   end Enable_Mouse_Callbacks;

   ----------------------------------------------------------------------------

   procedure Load_Splash is
      use Sprite_Manager;
   begin
      Game_State := Game_Splash;
      Set_Frame_Size (Splash_Screen, 800.0, 600.0);
      Set_Number_Of_Frames (Splash_Screen, 1);
      Set_Active (Splash_Screen, True);
      Set_Visible (Splash_Screen, True);
      Add_Texture (Splash_Screen, "src/resources/splash.png", False);
   end Load_Splash;

   ----------------------------------------------------------------------------
   --  LoadTextures
   procedure Load_Sprites (Screen : in out Input_Callback.Callback_Window) is
      use GL.Types;
      use Sprite_Manager;
      Screen_Width    : Glfw.Size;
      Screen_Height   : Glfw.Size;
      VP_Width        : Size;
      VP_Height       : Size;
      Centre          : Point;
      Radius          : Float;
      Offset_Y        : Float;
      Collision       : Rectangle;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      VP_Width := Size (Screen_Width) - 2 * Border_Width;
      VP_Height := Size (Screen_Height) - 2 * Border_Width;
      GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

      Set_Frame_Size (Background, 1877.0, 600.0);
      Set_Number_Of_Frames (Background, 1);
      Add_Texture (Background, "src/resources/background.png", False);

      Player_Manager.Init_Players;

      Set_Visible (Background, True);
      Set_Active (Background, True);

      Input_Manager.Init_Buttons;
      Collision.Left := 34.0;
      Collision.Right := -10.0;
      Player_Manager.Set_Collision (Player_Manager.Robot_Left, Collision);
      Player_Manager.Set_Collision (Player_Manager.Robot_Right, Collision);

      Set_Frame_Size (Pickup, 26.0, 50.0);
      Set_Number_Of_Frames (Pickup, 1);
      Set_Value (Pickup, 50);
      Add_Texture (Pickup, "src/resources/oil.png", True);

      Set_Frame_Size (Enemy, 32.0, 50.0);
      Set_Number_Of_Frames (Enemy, 1);
      Set_Value (Enemy, -50);
      Set_Collidable (Enemy, True);
      Add_Texture (Enemy, "src/resources/water.png", True);

      Centre.X := 0.5 * Get_Size (Pickup).Width;
      Offset_Y := 3.0 * 0.25 * Get_Size (Pickup).Height;
      Centre.Y := Offset_Y;
      Set_Centre (Pickup, Centre);
      Radius := 0.5 * Get_Size (Pickup).Width;
      Set_Radius (Pickup, Radius);
      Player_Manager.Set_Collidable (Player_Manager.Robot_Left, True);
      Player_Manager.Set_Collidable (Player_Manager.Robot_Right, True);
      Set_Collidable (Pickup, True);

      Set_Frame_Size (Menu_Screen, 800.0, 600.0);
      Set_Number_Of_Frames (Menu_Screen, 1);
      Set_Active (Menu_Screen, True);
      Set_Visible (Menu_Screen, True);
      Add_Texture (Menu_Screen, "src/resources/main_menu.png", False);

      Set_Frame_Size (Credits_Screen, 800.0, 600.0);
      Set_Number_Of_Frames (Credits_Screen, 1);
      Set_Visible (Credits_Screen, True);
      Add_Texture (Credits_Screen, "src/resources/credits1.png", False);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Load_Sprites.");
         Put_Line (Exception_Information (anError));
         raise;
   end Load_Sprites;

   --  -------------------------------------------------------------------------

   procedure Process_Input (Delta_Time : Float) is
      use Input_Manager;
      use Player_Manager;
      use Sprite_Manager;
      Velocity            : constant Float := 50.0;
      Player              : constant Player_Index := Get_Current_Player;
      aCommand            : Command := Get_Current_Command;
      Continue            : Boolean := True;
   begin
      case Game_State is
         when Game_Splash | Game_Loading =>
            Continue := False;
         when Game_Menu | Game_Credits  | Game_Paused |
              Game_Next_Level | Game_Over =>
            aCommand := Command_UI;
         when  Game_Running => null;
      end case;

      if Continue then
         UI_Timer := UI_Timer + Delta_Time;
         if UI_Timer > UI_Threshold then
            UI_Timer := 0.0;
         end if;

         case aCommand is
            when Command_UI =>
               if Is_Clicked (Pause_Button) then
                  Set_Clicked (Pause_Button, False);
                  Set_Visible (Pause_Button, False);
                  Set_Active (Pause_Button, False);

                  Set_Visible (Resume_Button, True);
                  Set_Active (Resume_Button, True);
                  Game_State := Game_Paused;

               elsif Is_Clicked (Resume_Button) then
                  Set_Clicked (Resume_Button, False);
                  Set_Visible (Resume_Button, False);
                  Set_Active (Resume_Button, False);

                  Set_Visible (Pause_Button, True);
                  Set_Active (Pause_Button, True);
                  Game_State := Game_Running;

               elsif Is_Clicked (Play_Button) then
                  Set_Clicked (Play_Button, False);
                  Set_Active (Play_Button, False);
                  Set_Active (Exit_Button, False);
                  Set_Active (Credits_Button, False);
                  Game_State := Game_Running;

               elsif Is_Clicked (Credits_Button) then
                  Set_Clicked (Credits_Button, False);
                  Set_Active (Play_Button, False);
                  Set_Active (Exit_Button, False);
                  Set_Active (Credits_Button, False);
               Game_State := Game_Credits;

               elsif Is_Clicked (Exit_Button) then
                  Set_Clicked (Exit_Button, False);
                  Set_Active (Play_Button, False);
                  Set_Active (Exit_Button, False);
                  Set_Active (Credits_Button, False);

               elsif Is_Clicked (Menu_Button) then
                  Set_Clicked (Menu_Button, False);
                  Set_Active (Menu_Button, False);
                  Game_State := Game_Menu;
               end if;

            when Command_Left =>
               if Player = Robot_Right then
                  Set_Active (Robot_Right, False);
                  Set_Visible (Robot_Right, False);
                  Set_Position (Robot_Left, Get_Position (Robot_Right));
               end if;
               Set_Current_Player (Robot_Left);
               Set_Active (Robot_Left, True);
               Set_Visible (Robot_Left, True);
               Set_Velocity (Robot_Left, -Velocity);
               Set_Velocity (Background, Velocity);
            when Command_Right =>
               if Player = Robot_Left then
                  Set_Active (Robot_Left, False);
                  Set_Visible (Robot_Left, False);
                  Set_Position (Robot_Right, Get_Position (Robot_Left));
               end if;
               Set_Current_Player (Robot_Right);
               Set_Active (Robot_Right, True);
               Set_Visible (Robot_Right, True);
               Set_Velocity (Robot_Right, Velocity);
               Set_Velocity (Background, -Velocity);
            when Command_Stop =>
               Set_Velocity (Background, 0.0);
               Set_Velocity (Player, 0.0);
            when Command_Up => Jump (Player, Sprite_Up);
            when Command_Down => Jump (Player, Sprite_Down);
            when Command_Invalid => null;
         end case;
         Set_Command_Invalid;
      end if;
   end Process_Input;

   --  -------------------------------------------------------------------------

   procedure Render_Sprites (Screen : in out Input_Callback.Callback_Window) is
   begin
      Utilities.Clear_Colour;
      Sprite_Manager.Clear_Buffers;
      Resize_GL_Scene (Screen);
      GL.Objects.Programs.Use_Program (Game_Program);
      GL.Uniforms.Set_Single
        (Model_Uniform, GL.Types.Singles.Identity4);
      GL.Uniforms.Set_Int (Texture_Uniform, 0);

      case Game_State is
         when Game_Splash | Game_Loading  =>
            Sprite_Manager.Render (Splash_Screen);

         when Game_Menu =>
            Sprite_Manager.Render (Menu_Screen);
            Input_Manager.Render_Button (Input_Manager.Play_Button);
            Input_Manager.Render_Button (Input_Manager.Credits_Button);
            Input_Manager.Render_Button (Input_Manager.Exit_Button);

         when Game_Credits =>
            Sprite_Manager.Render (Credits_Screen);
            Input_Manager.Render_Button (Input_Manager.Menu_Button);

         when Game_Next_Level | Game_Over => null;

         when  Game_Running | Game_Paused =>
            Sprite_Manager.Render (Background);
            Player_Manager.Render_Players;
            Input_Manager.Render_Button (Input_Manager.Pause_Button);
            Input_Manager.Render_Button (Input_Manager.Resume_Button);
            Sprite_Manager.Render (Pickup);
            Sprite_Manager.Render (Enemy);
            Draw_Score;
      end case;
   end Render_Sprites;

   --  ------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window) is
      use GL.Objects.Programs;
      use GL.Types;
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
      VP_Width          : Size;
      VP_Height         : Size;
      Projection_Matrix : Singles.Matrix4 := Singles.Identity4;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      VP_Width := Size (Screen_Width) - 2 * Border_Width;
      VP_Height := Size (Screen_Height) - 2 * Border_Width;
      GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

      Maths.Init_Orthographic_Transform
        (Single (VP_Height), 0.0, 0.0, Single (VP_Width), 0.0, 1.0,
         Projection_Matrix);

      Use_Program (Game_Program);
      GL.Uniforms.Set_Single (Projection_Uniform, Projection_Matrix);
   end Resize_GL_Scene;

   --  ------------------------------------------------------------------------

   procedure Spawn_Enemy (Screen     : in out Input_Callback.Callback_Window;
                          Delta_Time : Float) is
      use Sprite_Manager;
      use Player_Manager;
      Screen_Width  : Glfw.Size;
      Screen_Height : Glfw.Size;
      Spawn_Margins : Object_Size;
      Player_Size   : Object_Size;
      Spawn_X       : Float;
      Spawn_Y       : Float;
   begin
      if not Is_Visible (Enemy) then
         Enemy_Spawn_Timer := Enemy_Spawn_Timer + Delta_Time;
         if Enemy_Spawn_Timer > Enemy_Spawn_Threshold then
            Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
            Spawn_Margins := Get_Size (Enemy);
            Spawn_X := Abs (Float (Maths.Random_Float)) *
              (Float (Screen_Width) - 2.0 * Spawn_Margins.Width);
            Player_Size := Get_Size (Get_Current_Player);
            Spawn_Y :=
              Float (Screen_Height) - Spawn_Margins.Height
              - (Abs (Float (Maths.Random_Float)) *
                   Player_Size.Height + 2.0 * Spawn_Margins.Height);
            Set_Position (Enemy, Spawn_X, Spawn_Y);
            Set_Visible (Enemy, True);
            Set_Active (Enemy, True);
         end if;
      end if;
   end Spawn_Enemy;

   --  ------------------------------------------------------------------------

   procedure Spawn_Pickup (Screen     : in out Input_Callback.Callback_Window;
                           Delta_Time : Float) is
      use Sprite_Manager;
      use Player_Manager;
      Screen_Width  : Glfw.Size;
      Screen_Height : Glfw.Size;
      Spawn_Margins : Object_Size;
      Player_Size   : Object_Size;
      Spawn_X       : Float;
      Spawn_Y       : Float;
   begin
      if not Is_Visible (Pickup) then
         Pickup_Spawn_Timer := Pickup_Spawn_Timer + Delta_Time;
         if Pickup_Spawn_Timer > Pickup_Spawn_Threshold then
            Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
            Spawn_Margins := Get_Size (Pickup);
            Spawn_X := Abs (Float (Maths.Random_Float)) *
              (Float (Screen_Width) - 2.0 * Spawn_Margins.Width);
            Player_Size := Get_Size (Get_Current_Player);
            Spawn_Y :=
              Float (Screen_Height) - Spawn_Margins.Height
              - (Abs (Float (Maths.Random_Float)) *
                   Player_Size.Height + 1.5 * Spawn_Margins.Height);
            Set_Position (Pickup, Spawn_X, Spawn_Y);
            Set_Visible (Pickup, True);
            Set_Active (Pickup, True);
            Pickup_Spawn_Timer := 0.0;
         end if;
      end if;
   end Spawn_Pickup;

   --  ------------------------------------------------------------------------

   procedure Start_Game (Screen : in out Input_Callback.Callback_Window) is
      use Program_Loader;
      use GL.Objects.Shaders;
      use GL.Types;
      use Glfw.Input;
      Window_Width       : Glfw.Size;
      Window_Height      : Glfw.Size;
   begin
      --          Screen.Set_Input_Toggle (Sticky_Keys, True);
      Screen.Set_Cursor_Mode (Mouse.Normal);
      Screen'Access.Get_Size (Window_Width, Window_Height);
      Screen'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));

      Utilities.Clear_Background_Colour (Back);
      Input_Callback.Clear_All_Keys;
      Game_Program := Program_From
        ((Src ("src/shaders/robo2d_vertext_gui_shader.glsl", Vertex_Shader),
         Src ("src/shaders/robo2d_fragment_gui_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Game_Program);

      Model_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "model_matrix");
      Projection_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "projection_matrix");
      Texture_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "texture2d");

      Sprite_Manager.Init;
      Enable_Mouse_Callbacks (Screen, True);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update (Window : in out Input_Callback.Callback_Window) is
      Current_Time : constant Float := Float (Glfw.Time);
      Delta_Time   : constant Float := Current_Time - Last_Time;
      X_Position   : Glfw.Input.Mouse.Coordinate := 0.00001;
      Y_Position   : Glfw.Input.Mouse.Coordinate := 0.00002;
   begin
      Last_Time := Current_Time;
      Window'Access.Get_Cursor_Pos (X_Position, Y_Position);

      case Game_State is
         when Game_Splash =>
            Load_Splash;
            Splash_Timer := 0.0;
            Game_State := Game_Loading;

         when Game_Loading =>
            Splash_Timer := Splash_Timer + Delta_Time;
            Load_Sprites (Window);
            Sprite_Manager.Update (Splash_Screen, Delta_Time);
            Splash_Timer := Splash_Timer + Delta_Time;
            if Splash_Timer > Splash_Threshold then
               Game_State := Game_Menu;
            end if;

         when Game_Menu =>
            Put_Line ("Main_Loop.Update Game_Menu.");
            Sprite_Manager.Update (Menu_Screen, Delta_Time);
            Input_Manager.Set_Active (Input_Manager.Play_Button, True);
            Input_Manager.Set_Active (Input_Manager.Credits_Button, True);
            Input_Manager.Set_Active (Input_Manager.Exit_Button, True);
            Input_Manager.Update (Input_Manager.Play_Button, Delta_Time);
            Input_Manager.Update (Input_Manager.Credits_Button, Delta_Time);
            Input_Manager.Update (Input_Manager.Exit_Button, Delta_Time);
            Input_Manager.Update (Delta_Time);
            Process_Input (Delta_Time);

         when Game_Credits =>
            Sprite_Manager.Update (Credits_Screen, Delta_Time);
            Input_Manager.Set_Active (Input_Manager.Menu_Button, True);
            Input_Manager.Update (Input_Manager.Menu_Button, Delta_Time);
            Input_Manager.Update (Delta_Time);
            Process_Input (Delta_Time);

         when Game_Paused | Game_Next_Level | Game_Over =>
            Input_Manager.Update (Delta_Time);
            Process_Input (Delta_Time);

         when Game_Running =>
            Sprite_Manager.Update (Background, Delta_Time);
            Player_Manager.Update (Delta_Time);
            Input_Manager.Update (Input_Manager.Pause_Button, Delta_Time);
            Input_Manager.Update (Input_Manager.Resume_Button, Delta_Time);
            Sprite_Manager.Update (Pickup, Delta_Time);
            Spawn_Pickup (Window, Delta_Time);
            Sprite_Manager.Update (Enemy, Delta_Time);
            Spawn_Enemy (Window, Delta_Time);
            Check_Collisions;
      end case;

      Render_Sprites (Window);
      Input_Manager.Update_Command (Window);
      Process_Input (Delta_Time);
      Check_Boundaries (Window, Player_Manager.Get_Current_Player);
      Check_Background (Window);

   end Update;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game (Main_Window);
   while Running loop
      Update (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
   when anError : others =>
      Put_Line ("An exception occurred in Main_Loop.");
      Put_Line (Exception_Information (anError));
      raise;
end Main_Loop;
