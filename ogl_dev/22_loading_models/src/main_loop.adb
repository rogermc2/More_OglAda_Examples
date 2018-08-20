
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
with GL.Objects;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows.Context;

with Maths;
with Utilities;

with Ogldev_Basic_Lighting;
with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;

with Mesh_22;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
    use GL.Types;

    Background             : constant GL.Types.Colors.Color := (0.4, 0.4, 0.4, 0.0);

    VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Game_Camera            : Ogldev_Camera.Camera;
    Light_Technique        : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
    Direct_Light           : Ogldev_Lights_Common.Directional_Light;
    Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;

    Scale                  : Single := 0.0;

    --  ------------------------------------------------------------------------

    procedure Init (Window : in out Glfw.Windows.Window;
                    theMesh : out Mesh_22.Mesh; Result : out Boolean) is

        Window_Width        : Glfw.Size;
        Window_Height       : Glfw.Size;
        Position            : constant Singles.Vector3 := (3.0, 2.0, 10.0); --  Normalized by Camera.Init
        Target              : constant Singles.Vector3 := (0.0, 0.0, 0.1);  --  Normalized by Camera.Init
        Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
    begin
        VAO.Initialize_Id;
        VAO.Bind;
        Result := Ogldev_Basic_Lighting.Init (Light_Technique);
        if Result then
            Ogldev_Lights_Common.Init_Directional_Light (Direct_Light, 1.0, 0.01,
                                                         (1.0, 1.0, 1.0), (1.0, -1.0, 0.0));
            Window.Get_Framebuffer_Size (Window_Width, Window_Height);
            Ogldev_Camera.Init_Camera (Game_Camera, Int (Window_Width), Int (Window_Height),
                                       Position, Target, Up);
            Utilities.Clear_Background_Colour (Background);
            GL.Culling.Set_Front_Face (Clockwise);
            GL.Culling.Set_Cull_Face (GL.Culling.Back);
            GL.Toggles.Enable (GL.Toggles.Cull_Face);
            GL.Objects.Programs.Use_Program (Ogldev_Basic_Lighting.Lighting_Program (Light_Technique));
            Ogldev_Basic_Lighting.Set_Color_Texture_Unit
              (Light_Technique, Ogldev_Engine_Common.Colour_Texture_Unit_Index);

            Mesh_22.Load_Mesh (theMesh, "/Ada_Source/OglAda_Examples/ogl_dev/content/phoenix_ugv.md2");

            Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);
            Window.Set_Cursor_Mode (Glfw.Input.Mouse.Disabled);
            Glfw.Input.Poll_Events;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Init.");
            raise;
    end Init;

    --  ------------------------------------------------------------------------

    procedure Render_Scene (Window : in out Glfw.Windows.Window;
                            theMesh : Mesh_22.Mesh) is
        use Maths.Single_Math_Functions;
        use Ogldev_Basic_Lighting;
        use Ogldev_Camera;
        use Ogldev_Lights_Common;
        Window_Width         : Glfw.Size;
        Window_Height        : Glfw.Size;
        Field_Depth          : constant Single := 10.0;
        Point                : Point_Light_Array (1 .. 2);
        Spot                 : Spot_Light;
        Pipe                 : Ogldev_Pipeline.Pipeline;
    begin
        Scale := Scale + 0.1;
        Update_Camera (Game_Camera, Window);
        Utilities.Clear_Background_Colour_And_Depth (Background);

        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                                GL.Types.Int (Window_Height));

        Set_Diffuse_Intensity (Point (1), 0.25);
        Set_Point_Light (Point (1), (3.0, 1.0, Field_Depth * (Cos (Scale) + 1.0) / 2.0), (1.0, 0.5, 0.0));
        Set_Linear_Attenuation (Point (1), 0.1);

        Set_Diffuse_Intensity (Point (2), 0.25);
        Set_Point_Light (Point (2), (7.0, 1.0, Field_Depth * (Sin (Scale) + 1.0) / 2.0), (1.0, 0.5, 0.0));
        Set_Linear_Attenuation (Point (2), 0.1);
        Set_Point_Lights (Light_Technique, Point);

        Set_Diffuse_Intensity (Spot, 0.9);
        Set_Spot_Light (Spot, Get_Position (Game_Camera), (0.0, 1.0, 1.0));
        Set_Direction (Spot, Get_Target (Game_Camera));
        Set_Attenuation_Constant (Spot, 0.1);
        Set_Cut_Off (Spot, 10.0);
        Set_Spot_Lights (Light_Technique, Spot);

        Perspective_Proj_Info.Width := GL.Types.UInt (Window_Width);
        Perspective_Proj_Info.Height := GL.Types.UInt (Window_Height);

        Perspective_Proj_Info.FOV := 60.0;
        Perspective_Proj_Info.Height := GL.Types.UInt (Window_Height);
        Perspective_Proj_Info.Width := GL.Types.UInt (Window_Width);
        Perspective_Proj_Info.Z_Near := 1.0;
        Perspective_Proj_Info.Z_Far := 100.0;

        Ogldev_Pipeline.Set_Scale (Pipe, 0.04, 0.04, 0.04);
        Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
        Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, -10.0);
        Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),  Get_Target (Game_Camera),
                                    Get_Up (Game_Camera));
        Ogldev_Pipeline.Set_Perspective_Proj (Pipe, Perspective_Proj_Info);

        Set_WVP (Light_Technique, Ogldev_Pipeline.Get_WVP_Transform (pipe));
        Set_World_Matrix (Light_Technique, Ogldev_Pipeline.Get_World_Transform (pipe));
        Set_Directional_Light (Light_Technique, Direct_Light);
        Set_Eye_World_Pos (Light_Technique, Get_Position (Game_Camera));
        Set_Mat_Specular_Intensity (Light_Technique, 0.0);
        Set_Mat_Specular_Power (Light_Technique, 0);

        Mesh_22.Render_Mesh (theMesh);

    exception
        when  others =>
            Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
            raise;
    end Render_Scene;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    theMesh : Mesh_22.Mesh;
    Running : Boolean;
begin
    Init (Main_Window, theMesh, Running);
    while Running loop
        Render_Scene (Main_Window, theMesh);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
