
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Utilities;
--  with VBO_Indexer;

with Particle_System;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Dark_Blue              : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);
    Vertices_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Billboard_Buffer       : GL.Objects.Buffers.Buffer;
    Last_Time              : GL.Types.Single := GL.Types.Single (Glfw.Time);

    --  ------------------------------------------------------------------------

    procedure Load_Buffers is
        use GL.Objects.Buffers;
        use GL.Types;
        Vertex_Data        : constant Singles.Vector3_Array (1 .. 4) :=
                               ((-0.5, -0.5, 0.0),
                                (0.5, -0.5, 0.0),
                                (-0.5, 0.5, 0.0),
                                (0.5, 0.5, 0.0));
    begin
        Billboard_Buffer.Initialize_Id;
        Array_Buffer.Bind (Billboard_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Load_Buffers.");
            raise;
    end Load_Buffers;

    --  ------------------------------------------------------------------------

    procedure Load_Matrices (Window  : in out Glfw.Windows.Window) is
        use GL.Types;
        use GL.Types.Singles;
        View_Matrix       : Matrix4;
        Projection_Matrix : Matrix4;
        VP_Matrix         : Matrix4;
    begin
        Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
        VP_Matrix :=  Projection_Matrix * View_Matrix;
        Particle_System.Set_IDs (VP_Matrix);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Load_Matrices.");
            raise;
    end Load_Matrices;

    --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        use GL.Types;
        Current_Time : constant Single := Single (Glfw.Time);
        Delta_Time   : constant Single := Last_Time - Current_Time;
    begin
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        Particle_System.Update_Particles (Delta_Time);
        Load_Matrices (Window);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Billboard_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);
        GL.Attributes.Vertex_Attrib_Divisor (0, 0);
        Particle_System.Render_Particles;

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
        Window_Width    : constant Glfw.Size := 1024;
        Window_Height   : constant Glfw.Size := 768;

    begin
        Window.Set_Input_Toggle (Sticky_Keys, True);
        Window.Set_Cursor_Mode (Mouse.Disabled);

        Window'Access.Set_Size (Window_Width, Window_Height);
        Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                      Mouse.Coordinate (0.5 * Single (Window_Height)));
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

        Particle_System.Init;
        Load_Buffers;
        Last_Time := Single (Glfw.Time);
    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running         : Boolean := True;
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
