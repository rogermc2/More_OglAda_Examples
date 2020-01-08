
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
--  with  GL.Culling;
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

    procedure Draw (Window_Width, Window_Height : Glfw.Size;
                    thePosition : Sphere_Position);
    procedure Init_Lights (Render_Program : GL.Objects.Programs.Program);
    procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                            Render_Program : GL.Objects.Programs.Program;
                            thePosition : Sphere_Position);

    --  ------------------------------------------------------------------------

    procedure Init_GL is
        use GL.Pixels;
    begin
        Set_Unpack_Alignment (Words);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
--          GL.Culling.Set_Cull_Face (GL.Culling.Front);
--          GL.Toggles.Enable (GL.Toggles.Cull_Face);

        Utilities.Clear_Background_Colour (Black);
        GL.Buffers.Clear_Stencil_Buffer (0);
        GL.Buffers.Clear_Depth_Buffer (1.0);

        GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
    end Init_GL;

    --  ------------------------------------------------------------------------

    procedure Init_Lights (Render_Program : GL.Objects.Programs.Program) is
        use Shader_Manager;
        Ambient  : constant GL.Types.Singles.Vector4 := (0.3, 0.3, 0.3, 1.0);  -- ambient light
        Diffuse  : constant GL.Types.Singles.Vector4 := (0.7, 0.7, 0.7, 1.0);  -- diffuse light
        Specular : constant GL.Types.Singles.Vector4 := (1.0, 1.0, 1.0, 1.0);  -- specular light
--          Direct   : constant Singles.Vector4 := (0.0, 0.0, 1.0, 0.0); --  directional light
     begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Set_Light_Ambient (Ambient);
        Set_Light_Diffuse (Diffuse);
--          Set_Light_Direct (Direct);
        Set_Light_Specular (Specular);
    end Init_Lights;

    --  ------------------------------------------------------------------------

    procedure Display (Window : in out Glfw.Windows.Window) is
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
    begin
        --  Clear (GL_DEPTH_BUFFER_BIT, GL_STENCIL_BUFFER_BIT, COLOR_BUFFER_BIT)
        GL.Buffers.Clear ((True, False, True, True));
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));

        GL.Objects.Programs.Use_Program (Render_Program);

        GL.Objects.Textures.Targets.Texture_2D.Bind (Earth_Texture);

        GL.Objects.Textures.Set_Active_Unit (0);
        GL.Objects.Textures.Targets.Texture_2D.Bind (Earth_Texture);

        Draw (Window_Width, Window_Height, Left_Sphere);
        Draw (Window_Width, Window_Height, Centre_Sphere);
        Draw (Window_Width, Window_Height, Right_Sphere);

    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Display;

    --  ------------------------------------------------------------------------

    procedure Draw  (Window_Width, Window_Height : Glfw.Size;
                     thePosition : Sphere_Position) is
        use GL.Objects.Buffers;
        Vertex_Buffer : Buffer;
        Index_Buffer  : Buffer;
        theSphere     : Sphere.Sphere;
        Stride        : constant Int := Sphere.Get_Interleaved_Stride;
    begin

        Set_Matrices (Window_Width, Window_Height, Render_Program, thePosition);
        case thePosition is
            when Left_Sphere =>
                theSphere := Sphere_1;
                Vertex_Buffer := Vertex_Buffer_1;
                Index_Buffer := Index_Buffer_1;
                Shader_Manager.Set_Texture_Used (False);
            when Centre_Sphere => theSphere := Sphere_2;
                 Vertex_Buffer := Vertex_Buffer_2;
                 Index_Buffer := Index_Buffer_2;
                 Shader_Manager.Set_Texture_Used (False);
            when Right_Sphere => theSphere := Sphere_2;
                 Vertex_Buffer := Vertex_Buffer_2;
                 Index_Buffer := Index_Buffer_2;
                 Shader_Manager.Set_Texture_Used (True);
        end case;

        --  First attribute buffer : vertices
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, Stride, 0);

        --  Second attribute buffer : Normals
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, True, Stride, 3);

        --  Second attribute buffer : Tex Coords
        GL.Attributes.Enable_Vertex_Attrib_Array (2);
        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 2, Single_Type, True, Stride, 6);

        Element_Array_Buffer.Bind (Index_Buffer);
        GL.Objects.Buffers.Draw_Elements (Triangles, Sphere.Get_Indices_Size (theSphere),
                                           UInt_Type, 0);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
        GL.Attributes.Disable_Vertex_Attrib_Array (2);

    end Draw;

    --  ------------------------------------------------------------------------

    procedure Set_Matrices (Window_Width, Window_Height : Glfw.Size;
                            Render_Program : GL.Objects.Programs.Program;
                            thePosition : Sphere_Position) is
        use GL.Types.Singles;
        use Maths;
        Camera_Distance    : constant GL.Types.Single := 4.0;
        Camera_Angle_X      : constant Single := 0.0;
        Camera_Angle_Y      : constant Single := 0.0;
        Matrix_Model_Common : Matrix4 := Identity4;
        Matrix_Model_View   : Matrix4 := Identity4;
        View_Matrix         : constant Matrix4 :=
                                Translation_Matrix ((0.0, 0.0, -Camera_Distance));
        Normal_Matrix       : Matrix4 := Identity4;
        Projection_Matrix   : Matrix4;
        MVP_Matrix          : Matrix4;
    begin
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (-90.0), (1.0, 0.0, 0.0)) * Matrix_Model_Common;
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (Camera_Angle_Y), (0.0, 1.0, 0.0)) * Matrix_Model_Common;
        Matrix_Model_Common :=
          Rotation_Matrix (Degree (Camera_Angle_X), (1.0, 0.0, 0.0)) * Matrix_Model_Common;
        Init_Perspective_Transform (Degree (40.0), Single (Window_Width),
                                    Single (Window_Height),
                                    0.1, 100.0, Projection_Matrix);

        case thePosition is
            when Left_Sphere =>
                Matrix_Model_Common :=
                   Translation_Matrix ((-2.5, 0.0, 0.0)) * Matrix_Model_Common;
            when Centre_Sphere =>
                null;
            when Right_Sphere =>
                Matrix_Model_Common :=
                  Translation_Matrix ((2.5, 0.0, 0.0)) * Matrix_Model_Common;
        end case;
        Matrix_Model_View := View_Matrix * Matrix_Model_Common;

        Normal_Matrix := Matrix_Model_View;
        Normal_Matrix (GL.X, GL.W) := 0.0;
        Normal_Matrix (GL.Y, GL.W) := 0.0;
        Normal_Matrix (GL.Z, GL.W) := 0.0;
        Normal_Matrix (GL.W, GL.W) := 1.0;

        MVP_Matrix :=  Projection_Matrix * Matrix_Model_View;

        GL.Objects.Programs.Use_Program (Render_Program);
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
        Init_Lights (Render_Program);

        Sphere.Init (theSphere => Sphere_1, Radius => 1.0,
                     Sector_Count => 36, Stack_Count => 18, Smooth => False);
        Sphere.Init (Sphere_2);
        --  Create Buffers after sphere initialization
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
        Display (Main_Window);
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
