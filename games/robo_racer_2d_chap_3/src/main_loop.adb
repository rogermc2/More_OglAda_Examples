
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Player_Manager;
with Sprite_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Back               : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 1.0);
   Last_Time          : Float := 0.0;
   Game_Program       : GL.Objects.Programs.Program;
   Model_Uniform      : GL.Uniforms.Uniform;
   Projection_Uniform : GL.Uniforms.Uniform;
   Texture_Uniform    : GL.Uniforms.Uniform;
   Background         : Sprite_Manager.Sprite;

   procedure Resize_GL_Scene  (Screen : in out Glfw.Windows.Window);
   procedure Update_Sprites (Delta_Time : Float);

   --  ----------------------------------------------------------------------------

   procedure Load_Sprites (Screen : in out Glfw.Windows.Window) is
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

      Player_Manager.Init_Players;

      Set_Visible (Background, True);
      Set_Active (Background, True);
      Set_Velocity (Background, -50.0);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Load_Sprites.");
         Put_Line (Exception_Information (anError));
         raise;
   end Load_Sprites;

   --  -------------------------------------------------------------------------

   procedure Render_Sprites (Screen : in out Glfw.Windows.Window) is
   begin
      Utilities.Clear_Colour;
      Sprite_Manager.Clear_Buffers;
      Resize_GL_Scene (Screen);
      GL.Objects.Programs.Use_Program (Game_Program);
      GL.Uniforms.Set_Single (Model_Uniform, GL.Types.Singles.Identity4);
      GL.Uniforms.Set_Int (Texture_Uniform, 0);

      Sprite_Manager.Render (Background);
      Player_Manager.Render_Players;
   end Render_Sprites;

   --  ----------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Glfw.Windows.Window) is
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

   procedure Start_Game (Screen : in out Glfw.Windows.Window) is
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

   procedure Update_Game (Screen : in out Glfw.Windows.Window) is
      Current_Time : constant Float := Float (Glfw.Time);
      Delta_Time   : constant Float := Current_Time - Last_Time;
   begin
      Last_Time := Current_Time;
      Update_Sprites (Delta_Time);
      Render_Sprites (Screen);
   end Update_Game;

   --  -------------------------------------------------------------------------

   procedure Update_Sprites (Delta_Time : Float) is
   begin
      Sprite_Manager.Update (Background, Delta_Time);
      Player_Manager.Update (Delta_Time);
   end Update_Sprites;

   --  ----------------------------------------------------------------------------

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
