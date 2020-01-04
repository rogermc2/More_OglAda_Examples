
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
--  with GL.Fixed.Lighting;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Maths;
with Utilities;

with Buffers_Manager;
with Shader_Manager;
with Sphere;
with Textures_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    type Sphere_Position is (Left_Sphere, Centre_Sphere, Right_Sphere);

    Black                    : constant GL.Types.Colors.Color :=
                                 (0.0, 0.0, 0.0, 0.0);
    Screen_Width             : constant Glfw.Size := 1500;
    Screen_Height            : constant Glfw.Size := 500;
    Camera_Distance          : constant GL.Types.Single := 4.0;
--      Text_Width               : constant GL.Types.Int := 8;
--      Text_Height              : constant GL.Types.Int := 13;
--      Draw_Mode : constant GL.Types.Int := 0;

    Sphere_1                 : Sphere.Sphere;
    Sphere_2                 : Sphere.Sphere;

    Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer_1          : GL.Objects.Buffers.Buffer;
    Vertex_Buffer_2          : GL.Objects.Buffers.Buffer;
    Index_Buffer_1           : GL.Objects.Buffers.Buffer;
    Index_Buffer_2           : GL.Objects.Buffers.Buffer;
    Render_Program           : GL.Objects.Programs.Program;
    Earth_Texture            : GL.Objects.Textures.Texture;

    --  ------------------------------------------------------------------------

--      procedure Init_Lights;
    procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                            Render_Program : GL.Objects.Programs.Program;
                            Position : Sphere_Position);

    --  ------------------------------------------------------------------------

    procedure Init_GL is
--          use GL.Fixed.Lighting;
        use GL.Pixels;
    begin
--          Set_Shade_Model (Smooth);
        Set_Unpack_Alignment (Words);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
--          GL.Toggles.Enable (GL.Toggles.Lighting);
--          Put_Line ("Init_GL Lighting enabled.");
        GL.Toggles.Enable (GL.Toggles.Cull_Face);

        Utilities.Clear_Background_Colour (Black);
        GL.Buffers.Clear_Stencil_Buffer (0);
        GL.Buffers.Clear_Depth_Buffer (1.0);

        GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
--          Put_Line ("Init_GL Depth_Function set.");

--          Init_Lights;
--          Put_Line ("Init_GL Lights initialised.");
    end Init_GL;

    --  ------------------------------------------------------------------------

--      procedure Init_Lights is
--          use GL.Fixed.Lighting;
--          Ambient  : constant Colors.Color := (0.3, 0.3, 0.3, 1.0);    -- ambient light
--          Diffuse  : constant Colors.Color := (0.7, 0.7, 0.7, 1.0);    -- diffuse light
--          Specular : constant Colors.Color := (1.0, 1.0, 1.0, 1.0);    -- specular light
--          Position : constant Singles.Vector4 := (0.0, 0.0, 1.0, 0.0); --  directional light
--       begin
--          Put_Line ("Init_Lights entered.");
--          Set_Ambient (Light (0), Ambient);
--          Put_Line ("Init_Lights Ambient set.");
--          Set_Diffuse (Light (0), Diffuse);
--          Set_Specular (Light (0), Specular);
--          Set_Position (Light (0), Position);
--          Put_Line ("Init_Lights Light (0) set.");
--          GL.Toggles.Enable (GL.Toggles.Light0);
--
--      end Init_Lights;

    --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
--          Stride            : constant Int := 32;   -- bytes count
        Stride            : constant Int := Sphere.Get_Interleaved_Stride;
    begin
        --  Clear (GL_DEPTH_BUFFER_BIT, GL_STENCIL_BUFFER_BIT, COLOR_BUFFER_BIT)
        GL.Buffers.Clear ((True, False, True, True));
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);

        GL.Objects.Programs.Use_Program (Render_Program);

        Set_Matrices (Window_Width, Window_Height, Render_Program, Left_Sphere);
        GL.Objects.Textures.Targets.Texture_2D.Bind (Earth_Texture);

        GL.Objects.Textures.Set_Active_Unit (0);
        GL.Objects.Textures.Targets.Texture_2D.Bind (Earth_Texture);

        Shader_Manager.Set_Texture_Used (False);

        --  First attribute buffer : vertices
        Array_Buffer.Bind (Vertex_Buffer_1);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, Stride, 0);

        --  Second attribute buffer : Normals
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, True, Stride, 3);

        --  Second attribute buffer : Tex Coords
        GL.Attributes.Enable_Vertex_Attrib_Array (2);
        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 2, Single_Type, True, Stride, 6);

        Element_Array_Buffer.Bind (Index_Buffer_1);
        GL.Objects.Buffers.Draw_Elements (Triangles, Sphere.Get_Indices_Size (Sphere_1),
                                           UInt_Type, 0);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
        GL.Attributes.Disable_Vertex_Attrib_Array (2);

    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                            Render_Program : GL.Objects.Programs.Program;
                            Position : Sphere_Position) is
        use GL.Types.Singles;
        use Maths;
        --  Camera position, Look_At and Up are world coordinates.
--          Camera_Position    : constant Vector3 := (4.0, 3.0, -3.0);
        Camera_Angle_X     : constant Single := 0.0;
        Camera_Angle_Y     : constant Single := 0.0;
--          Look_At            : constant Vector3 := (0.0, 0.0, 0.0);
--          Up                 : constant Vector3 := (0.0, 1.0, 0.0);
        --  The Model_Matrix operates in world coordinates.
        Matrix_Model_Common : Matrix4 := Identity4;
--          Matrix_Model_Left   : Matrix4 := Identity4;
--          Matrix_Model_Centre : Matrix4 := Identity4;
--          Matrix_Model_Right  : Matrix4 := Identity4;
        Matrix_Model_View   : Matrix4 :=  Identity4;
        --  The View_Matrix transforms the world_cordinates of the world view
        --  into view (camera) coordinates.
        View_Matrix       : Matrix4 :=  Identity4;
        Normal_Matrix     : Matrix4 :=  Identity4;
        --  The Projection_Matrix projects the camera view in camera coordinates
        --  onto the camera view's Near plane
        Projection_Matrix : Matrix4;
        MVP_Matrix        : Matrix4;
    begin
        GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));
        View_Matrix := Translation_Matrix ((0.0, 0.0, -Camera_Distance)) * View_Matrix;
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (90.0), (1.0, 0.0, 0.0)) * Matrix_Model_Common;
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (Camera_Angle_Y), (0.0, 1.0, 0.0)) * Matrix_Model_Common;
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (Camera_Angle_X), (1.0, 0.0, 0.0)) * Matrix_Model_Common;

        GL.Objects.Programs.Use_Program (Render_Program);

--          Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);
        Init_Perspective_Transform (Degree (40.0), Single (Window_Width),
                                    Single (Window_Height),
                                    0.1, 100.0, Projection_Matrix);
        case Position is
            when Left_Sphere =>
                Matrix_Model_Common :=
                  Translation_Matrix ((-2.5, 0.0, 0.0)) * Matrix_Model_Common;
                Matrix_Model_View := View_Matrix * Matrix_Model_Common;
                Shader_Manager.Set_Texture_Used (False);
            when Centre_Sphere =>
                Shader_Manager.Set_Texture_Used (False);
                Matrix_Model_View := View_Matrix * Matrix_Model_Common;
            when Right_Sphere =>
                Shader_Manager.Set_Texture_Used (True);
                Matrix_Model_Common :=
                  Translation_Matrix ((2.5, 0.0, 0.0)) * Matrix_Model_Common;
                Matrix_Model_View := View_Matrix * Matrix_Model_Common;
        end case;

        Normal_Matrix := Matrix_Model_View;
        Normal_Matrix (GL.X, GL.W) := 0.0;
        Normal_Matrix (GL.Y, GL.W) := 0.0;
        Normal_Matrix (GL.Z, GL.W) := 0.0;
        Normal_Matrix (GL.W, GL.W) := 1.0;

        MVP_Matrix :=  Projection_Matrix * Matrix_Model_View;

        Shader_Manager.Set_Matrix_Model_View (Matrix_Model_View);
        Shader_Manager.Set_Matrix_Model_View_Projection (MVP_Matrix);
        Shader_Manager.Set_Matrix_Normal (Normal_Matrix);

    exception
        when others =>
            Put_Line ("An exception occurred in Man_Loop.Set_Matrices.");
            raise;
    end Set_Matrices;

    --  ------------------------------------------------------------------------

    procedure Setup (Window : in out Glfw.Windows.Window) is
    begin
        Window.Set_Size (Screen_Width, Screen_Height);
        Init_GL;
        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

        Shader_Manager.Init (Render_Program);

        Sphere.Init (theSphere => Sphere_1, Radius => 1.0,
                     Sector_Count => 10, Stack_Count => 5, Smooth => False);
--                       Sector_Count => 36, Stack_Count => 18, Smooth => False);
        Sphere.Init (Sphere_2);

        Buffers_Manager.Create_Vertex_Buffers (Vertex_Buffer_1, Vertex_Buffer_2,
                                               Sphere_1, Sphere_2);
        Buffers_Manager.Create_Index_Buffers (Index_Buffer_1, Index_Buffer_2,
                                              Sphere_1, Sphere_2);

        Textures_Manager.Load_Texture (Earth_Texture, "src/earth2048.bmp", True);

    exception
        when others =>
            Put_Line ("An exception occurred in Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup (Main_Window);
    while Running loop
        Render (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
