
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Types;
--  with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Sprite_Manager;
with Textures_Manager;
with Vertex_Data;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   --     Background         : constant GL.Types.Colors.Color := ((0.6, 0.6, 0.6, 1.0));
   Textures           : Textures_Manager.Texture_List;
   Texture_Program    : GL.Objects.Programs.Program;
   Texture_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Quad_Buffer        : GL.Objects.Buffers.Buffer;
   Texture_Buffer     : GL.Objects.Buffers.Buffer;
   Texture_Uniform    : GL.Uniforms.Uniform;
   Robot_Left         : Sprite_Manager.Sprite;
   Robot_Right        : Sprite_Manager.Sprite;
   Robot_Left_Strip  : Sprite_Manager.Sprite;
   Robot_Right_Strip  : Sprite_Manager.Sprite;
   Background         : Sprite_Manager.Sprite;
   Player             : Sprite_Manager.Sprite;

   --  ----------------------------------------------------------------------------

   procedure Load_Sprites (Screen : in out Glfw.Windows.Window) is
        use Sprite_Manager;
        Screen_Width        : Glfw.Size;
        Screen_Height       : Glfw.Size;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);

      Set_Frame_Size (Background, 1877.0, 600.0);
      Set_Number_Of_Frames (Background, 1);
      Add_Texture (Background, "resources/background.png", False);

      Set_Frame_Size (Robot_Right, 100.0, 125.0);
      Set_Number_Of_Frames (Robot_Right, 4);
      Set_Position (Robot_Right, 0.0, Float (Screen_Height) - 130.0);
      Add_Texture (Robot_Right, "resources/robot_right_00.png");
      Add_Texture (Robot_Right, "resources/robot_right_01.png");
      Add_Texture (Robot_Right, "resources/robot_right_02.png");
      Add_Texture (Robot_Right, "resources/robot_right_03.png");

      Set_Frame_Size (Robot_Left, 100.0, 125.0);
      Set_Number_Of_Frames (Robot_Left, 4);
      Set_Position (Robot_Left, 0.0, Float (Screen_Height) - 130.0);
      Add_Texture (Robot_Left, "resources/robot_left_00.png");
      Add_Texture (Robot_Left, "resources/robot_left_01.png");
      Add_Texture (Robot_Left, "resources/robot_left_02.png");
      Add_Texture (Robot_Left, "resources/robot_left_03.png");

      Set_Frame_Size (Robot_Right_Strip, 125.0, 100.0);
      Set_Number_Of_Frames (Robot_Right_Strip, 4);
      Set_Position (Robot_Right_Strip, 0.0, Float (Screen_Height) - 130.0);
      Add_Texture (Robot_Right_Strip, "resources/robot_right_strip.png");

      Set_Frame_Size (Robot_Left_Strip, 125.0, 100.0);
      Set_Number_Of_Frames (Robot_Left_Strip, 4);
      Set_Position (Robot_Left_Strip, 0.0, Float (Screen_Height) - 130.0);
      Add_Texture (Robot_Left_Strip, "resources/robot_left_strip.png");

      Set_Visible (Background, True);
      Set_Active (Background, True);
      Set_Velocity (Background, -50.0);

      Set_Visible (Robot_Right, True);
      Set_Active (Robot_Right, True);
      Set_Velocity (Robot_Right, 50.0);

      Player := Robot_Right;

   end Load_Sprites;

   --  -------------------------------------------------------------------------

   procedure Draw_Texture is
   begin
      Texture_VAO.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 6);

   end Draw_Texture;

   --  -------------------------------------------------------------------------

   procedure Start_Game (Screen : in out Glfw.Windows.Window) is
      use GL.Attributes;
      use GL.Types;
      use Program_Loader;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Vertex_Data;
      Images   : Textures_Manager.Image_Sources (1 .. 1);

   begin
      --        GL.Buffers.Set_Color_Clear_Value (Background);

      Textures_Manager.Load_Textures (Textures, Images);
      Load_Sprites (Screen);

      Texture_Program := Program_From
        ((Src ("src/shaders/robo2d_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/robo2d_fragment_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Texture_Program);
      Texture_Uniform :=
        GL.Objects.Programs.Uniform_Location (Texture_Program, "texture2d");
      GL.Uniforms.Set_Int (Texture_Uniform, 0);

      Texture_VAO.Initialize_Id;
      Texture_VAO.Bind;

      Quad_Buffer.Initialize_Id;
      Array_Buffer.Bind (Quad_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Quad_Vertices,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (0);
      Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);

      Texture_Buffer.Initialize_Id;
      Array_Buffer.Bind (Texture_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Texture_Coords,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (1);
      Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);

      --        Textures_Manager.Load_Texture (aTexture        => aTexture,
      --                                       Image_File_Name => "src/flower.bmp",
      --                                       Wrap            => True);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update is
   begin
      Utilities.Clear_Colour;
      Draw_Texture;
   end Update;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
   Window   : Glfw.Windows.Window;
begin
   Start_Game (Window);
   while Running loop
      Update;
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
