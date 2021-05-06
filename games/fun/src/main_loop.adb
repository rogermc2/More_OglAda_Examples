
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Background         : constant GL.Types.Colors.Color := ((0.6, 0.6, 0.6, 1.0));
    Rendering_Program  : GL.Objects.Programs.Program;
    Vertex_Array       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer      : GL.Objects.Buffers.Buffer;
    Num_Dimension      : constant GL.Types.Int := 2;

    --  ----------------------------------------------------------------------------

    procedure Draw_Points is
        use GL.Types;
        Num_Points    : constant Int := 3;
    begin
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, Num_Points);

    exception
        when anError : others =>
            Put_Line ("An exceptiom occurred in Draw_Points.");
            Put_Line (Exception_Information (anError));
            raise;
    end Draw_Points;

    --  ----------------------------------------------------------------------------

    procedure Update is
        use GL.Types;
    begin
        Utilities.Clear_Colour;
        GL.Attributes.Set_Vertex_Attrib_Pointer  (0, Num_Dimension, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Draw_Points;
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError : others =>
            Put_Line ("An exceptiom occurred in Update.");
            Put_Line (Exception_Information (anError));
            raise;
    end Update;

    --  ----------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Types;
        use Program_Loader;
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        Vertex_Data : constant GL.Types.Singles.Vector2_Array (1 .. 3) :=
                        ((0.1, -0.6),
                         (0.7, -0.6),
                         (0.4, -0.1));
    begin
        GL.Buffers.Set_Color_Clear_Value (Background);
        Rendering_Program := Program_From (
                                           (Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
                                           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader))
                                          );
        GL.Objects.Programs.Use_Program (Rendering_Program);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);
        -- Point size is set in the vertex shader
        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
    exception
        when anError : others =>
            Put_Line ("An exceptiom occurred in Setup_Graphic.");
            Put_Line (Exception_Information (anError));
            raise;
    end Setup_Graphic;

    --  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
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
end Main_Loop;
