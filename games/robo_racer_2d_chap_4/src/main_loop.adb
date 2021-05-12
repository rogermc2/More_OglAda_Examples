
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;
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
with Sprite_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is

   Back               : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 1.0);
   Last_Time          : Float := 0.0;
   Game_Program       : GL.Objects.Programs.Program;
   Model_Uniform      : GL.Uniforms.Uniform;
   Projection_Uniform : GL.Uniforms.Uniform;
   Texture_Uniform    : GL.Uniforms.Uniform;
   Robot_Left         : Sprite_Manager.Sprite;
   Robot_Right        : Sprite_Manager.Sprite;
   Robot_Left_Strip   : Sprite_Manager.Sprite;
   Robot_Right_Strip  : Sprite_Manager.Sprite;
   Background         : Sprite_Manager.Sprite;
   Player             : Sprite_Manager.Sprite;

   procedure Resize_GL_Scene  (Screen : in out Input_Callback.Callback_Window);

   --  ----------------------------------------------------------------------------
   --  LoadTextures
   procedure Load_Sprites (Screen : in out Input_Callback.Callback_Window) is
      use GL.Types;
      use Sprite_Manager;
      Screen_Width    : Glfw.Size;
      Screen_Height   : Glfw.Size;
      Border_Width    : constant Size := 10;
      VP_Width        : Size;
      VP_Height       : Size;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      VP_Width := Size (Screen_Width) - 2 * Border_Width;
      VP_Height := Size (Screen_Height) - 2 * Border_Width;
      GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

      Set_Frame_Size (Background, 1877.0, 600.0);
      Set_Number_Of_Frames (Background, 1);
      Add_Texture (Background, "src/resources/background.png", False);

      Set_Frame_Size (Robot_Right, 100.0, 125.0);
      Set_Number_Of_Frames (Robot_Right, 4);
      Set_Position (Robot_Right, 10.0, 50.0);
      Add_Texture (Robot_Right, "src/resources/robot_right_00.png");
      Add_Texture (Robot_Right, "src/resources/robot_right_01.png");
      Add_Texture (Robot_Right, "src/resources/robot_right_02.png");
      Add_Texture (Robot_Right, "src/resources/robot_right_03.png");

      Set_Frame_Size (Robot_Left, 100.0, 125.0);
      Set_Number_Of_Frames (Robot_Left, 4);
      Set_Position (Robot_Left, 500.0, 50.0);
      Add_Texture (Robot_Left, "src/resources/robot_left_00.png");
      Add_Texture (Robot_Left, "src/resources/robot_left_01.png");
      Add_Texture (Robot_Left, "src/resources/robot_left_02.png");
      Add_Texture (Robot_Left, "src/resources/robot_left_03.png");

      Set_Frame_Size (Robot_Right_Strip, 125.0, 100.0);
      Set_Number_Of_Frames (Robot_Right_Strip, 4);
      Set_Position (Robot_Right_Strip, 0.0, 50.0);
      Add_Texture (Robot_Right_Strip, "src/resources/robot_right_strip.png");

      Set_Frame_Size (Robot_Left_Strip, 125.0, 100.0);
      Set_Number_Of_Frames (Robot_Left_Strip, 4);
      Set_Position (Robot_Left_Strip, 0.0, 50.0);
      Add_Texture (Robot_Left_Strip, "src/resources/robot_left_strip.png");

      Set_Visible (Background, True);
      Set_Active (Background, True);
--        Set_Velocity (Background, -50.0);

      Set_Visible (Robot_Right, True);
      Set_Active (Robot_Right, True);
      Set_Velocity (Robot_Right, 50.0);

      Player := Robot_Right;
      Set_Velocity (Player, 0.0);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Load_Sprites.");
         Put_Line (Exception_Information (anError));
         raise;
   end Load_Sprites;

   --  -------------------------------------------------------------------------

   procedure Process_Input is
      use Input_Manager;
      use Sprite_Manager;
   begin
      Put_Line ("Main_Loop.Process_Input Command: " &
                Command'Image (Get_Command));
      case Get_Command is
         when Command_Left =>
            if Player = Robot_Right then
               Set_Active (Robot_Right, False);
               Set_Visible (Robot_Right, False);
               Set_Position (Robot_Left, Get_Position (Robot_Right));
            end if;
            Player := Robot_Left;
            Set_Active (Player, True);
            Set_Visible (Player, True);
            Set_Velocity (Player, -50.0);
            Set_Velocity (Background, 50.0);
         when Command_Right =>
            if Player = Robot_Left then
               Set_Active (Robot_Left, False);
               Set_Visible (Robot_Left, False);
               Set_Position (Robot_Right, Get_Position (Robot_Left));
            end if;
            Player := Robot_Right;
            Set_Active (Player, True);
            Set_Visible (Player, True);
            Set_Velocity (Player, 50.0);
            Set_Velocity (Background, -50.0);
         when Command_Stop =>
            Set_Velocity (Background, 0.0);
            Set_Velocity (Player, 0.0);
         when Command_Up => Jump (Player, Sprite_Up);
         when  Command_Down => Jump (Player, Sprite_Down);
         when Command_Quit => null;
      end case;
   end Process_Input;

   --  -------------------------------------------------------------------------

   procedure Render_Sprites (Screen : in out Input_Callback.Callback_Window) is
   begin
      Utilities.Clear_Colour;
      Sprite_Manager.Clear_Buffers;
      Resize_GL_Scene (Screen);
      GL.Objects.Programs.Use_Program (Game_Program);
      GL.Uniforms.Set_Single (Model_Uniform, GL.Types.Singles.Identity4);
      GL.Uniforms.Set_Int (Texture_Uniform, 0);

      Sprite_Manager.Render (Background);
      Sprite_Manager.Render (Robot_Left);
      Sprite_Manager.Render (Robot_Right);
      Sprite_Manager.Render (Robot_Left_Strip);
      Sprite_Manager.Render (Robot_Right_Strip);
   end Render_Sprites;

   --  ----------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window) is
      use GL.Objects.Programs;
      use GL.Types;
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
      VP_Width          : Size;
      VP_Height         : Size;
      Border_Width      : constant Size := 2;
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

   --  ----------------------------------------------------------------------------

   procedure Start_Game (Screen : in out Input_Callback.Callback_Window) is
      use Program_Loader;
      use GL.Objects.Shaders;
   begin
      Utilities.Clear_Background_Colour (Back);
      Load_Sprites (Screen);

      Game_Program := Program_From
        ((Src ("src/shaders/robo2d_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/robo2d_fragment_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Game_Program);

      Model_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "model_matrix");
      Projection_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "projection_matrix");
      Texture_Uniform :=
        GL.Objects.Programs.Uniform_Location (Game_Program, "texture2d");

      Sprite_Manager.Init;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update (Window     : in out Input_Callback.Callback_Window;
                     Delta_Time : Float) is
   begin
      Input_Manager.Update (Window);
      Process_Input;
      Sprite_Manager.Update (Background, Delta_Time);
      Sprite_Manager.Update (Robot_Left, Delta_Time);
      Sprite_Manager.Update (Robot_Right, Delta_Time);
      Sprite_Manager.Update (Robot_Left_Strip, Delta_Time);
      Sprite_Manager.Update (Robot_Right_Strip, Delta_Time);
   end Update;

   --  ----------------------------------------------------------------------------

   procedure Update_Game (Window : in out Input_Callback.Callback_Window) is
      Current_Time : constant Float := Float (Glfw.Time);
      Delta_Time   : constant Float := Current_Time - Last_Time;
   begin
      Last_Time := Current_Time;
      Update (Window, Delta_Time);
      Render_Sprites (Window);
   end Update_Game;

   --  -------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game (Main_Window);
   while Running loop
      Update_Game (Main_Window);
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
