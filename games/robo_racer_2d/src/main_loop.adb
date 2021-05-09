
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

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
--  with Textures_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    --     Background         : constant GL.Types.Colors.Color := ((0.6, 0.6, 0.6, 1.0));
--      Textures           : Textures_Manager.Texture_List;
    Texture_Program    : GL.Objects.Programs.Program;
    Texture_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Quad_Buffer        : GL.Objects.Buffers.Buffer;
    Texture_Buffer     : GL.Objects.Buffers.Buffer;
    Texture_Uniform    : GL.Uniforms.Uniform;
    Robot_Left         : Sprite_Manager.Sprite;
    Robot_Right        : Sprite_Manager.Sprite;
    Robot_Left_Strip  : Sprite_Manager.Sprite;
    Robot_Right_Strip  : Sprite_Manager.Sprite;
--      Background         : Sprite_Manager.Sprite;
    Player             : Sprite_Manager.Sprite;

    --  ----------------------------------------------------------------------------

    procedure Load_Sprites (Screen : in out Glfw.Windows.Window) is
        use Sprite_Manager;
        Screen_Width        : Glfw.Size;
        Screen_Height       : Glfw.Size;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);

        Put_Line ("Load_Sprites Screen_Width and Screen_Height set.");
--          Set_Frame_Size (Background, 1877.0, 600.0);
--          Set_Number_Of_Frames (Background, 1);
--          Add_Texture (Background, "resources/background.png", False);

        Set_Frame_Size (Robot_Right, 100.0, 125.0);
        Set_Number_Of_Frames (Robot_Right, 4);
        Set_Position (Robot_Right, 0.0, Float (Screen_Height) - 130.0);
        Add_Texture (Robot_Right, "src/resources/robot_right_00.png");
        Add_Texture (Robot_Right, "src/resources/robot_right_01.png");
        Add_Texture (Robot_Right, "src/resources/robot_right_02.png");
        Add_Texture (Robot_Right, "src/resources/robot_right_03.png");

        Set_Frame_Size (Robot_Left, 100.0, 125.0);
        Set_Number_Of_Frames (Robot_Left, 4);
        Set_Position (Robot_Left, 0.0, Float (Screen_Height) - 130.0);
        Add_Texture (Robot_Left, "src/resources/robot_left_00.png");
        Add_Texture (Robot_Left, "src/resources/robot_left_01.png");
        Add_Texture (Robot_Left, "src/resources/robot_left_02.png");
        Add_Texture (Robot_Left, "src/resources/robot_left_03.png");

        Set_Frame_Size (Robot_Right_Strip, 125.0, 100.0);
        Set_Number_Of_Frames (Robot_Right_Strip, 4);
        Set_Position (Robot_Right_Strip, 0.0, Float (Screen_Height) - 130.0);
        Add_Texture (Robot_Right_Strip, "src/resources/robot_right_strip.png");

        Set_Frame_Size (Robot_Left_Strip, 125.0, 100.0);
        Set_Number_Of_Frames (Robot_Left_Strip, 4);
        Set_Position (Robot_Left_Strip, 0.0, Float (Screen_Height) - 130.0);
        Add_Texture (Robot_Left_Strip, "src/resources/robot_left_strip.png");

--          Set_Visible (Background, True);
--          Set_Active (Background, True);
--          Set_Velocity (Background, -50.0);

        Set_Visible (Robot_Right, True);
        Set_Active (Robot_Right, True);
        Set_Velocity (Robot_Right, 50.0);

        Player := Robot_Right;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Load_Sprites.");
            Put_Line (Exception_Information (anError));
            raise;
    end Load_Sprites;

    --  -------------------------------------------------------------------------

    procedure Draw_Texture is
    begin
        Texture_VAO.Bind;
        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 6);

    end Draw_Texture;

    --  -------------------------------------------------------------------------

    procedure Render is
    begin
        Utilities.Clear_Colour;
--          Sprite_Manager.Render (Background, Texture_Program, Texture_VAO);
        Sprite_Manager.Render (Robot_Left, Texture_Program, Texture_VAO);
        Sprite_Manager.Render (Robot_Right, Texture_Program, Texture_VAO);
        Sprite_Manager.Render (Robot_Left_Strip, Texture_Program, Texture_VAO);
        Sprite_Manager.Render (Robot_Right_Strip, Texture_Program, Texture_VAO);
    end Render;

    --  ----------------------------------------------------------------------------

    procedure Start_Game (Screen : in out Glfw.Windows.Window) is
        use Program_Loader;
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
--          Images   : Textures_Manager.Image_Sources (1 .. 1);

    begin
        --        GL.Buffers.Set_Color_Clear_Value (Background);

--          Textures_Manager.Load_Textures (Textures, Images);
        Load_Sprites (Screen);
        Put_Line ("Start_Game Sprites loaded.");

        Texture_Program := Program_From
          ((Src ("src/shaders/robo2d_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/robo2d_fragment_shader.glsl", Fragment_Shader)));
        Put_Line ("Start_Game Texture_Program loaded.");
        GL.Objects.Programs.Use_Program (Texture_Program);

        Texture_Uniform :=
          GL.Objects.Programs.Uniform_Location (Texture_Program, "texture2d");
        GL.Uniforms.Set_Int (Texture_Uniform, 0);

        Texture_VAO.Initialize_Id;
        Texture_VAO.Bind;

        Quad_Buffer.Initialize_Id;
        Array_Buffer.Bind (Quad_Buffer);

        Texture_Buffer.Initialize_Id;
        Array_Buffer.Bind (Texture_Buffer);

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
begin
    Start_Game (Main_Window);
    while Running loop
        Render;
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
