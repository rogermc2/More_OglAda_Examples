
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Toggles;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;
with Vertex_Data;

--  Main_Loop produces a 512x512 window centered on the display
--  showing a rotating tetrahedron
procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

    Vertex_Array    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer   : GL.Objects.Buffers.Buffer;
    Colour_Buffer   : GL.Objects.Buffers.Buffer;
    Render_Program  : GL.Objects.Programs.Program;

    Black : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);

    --  ------------------------------------------------------------------------

    procedure Draw_Scene (index : Int) is
        use GL.Objects.Buffers;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Utilities.Clear_Background_Colour (Black);

        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (0, 2, Single_Type, False, 0, 0);

        Array_Buffer.Bind (Colour_Buffer);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (1, 3, Single_Type, False, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Line_Loop, 0, index);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);

    exception
        when  others =>
            Put_Line ("An exceptiom occurred in Draw_Scene.");
            raise;
    end Draw_Scene;

    --  ------------------------------------------------------------------------

    procedure Setup is
        use GL.Objects.Buffers;
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Render_Program := Program_From
          ((Src ("src/shaders/tutorial3.vert", Vertex_Shader),
           Src ("src/shaders/tutorial3.frag", Fragment_Shader)));

        GL.Toggles.Enable (GL.Toggles.Multisample);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        --  Enable Z depth testing so objects closest to the viewpoint
        --  are in front of objects further away
        GL.Buffers.Set_Depth_Function (Less);

        Use_Program (Render_Program);

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer
          (Array_Buffer, Vertex_Data.Diamond_Vertices, Static_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        Utilities.Load_Vertex_Buffer
          (Array_Buffer, Vertex_Data.Colour_Data, Static_Draw);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
    Index   : Int := 0;
begin
    Setup;
    while Running loop
        Index := Index + 1;
        Draw_Scene (Index);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Delay (1.0);
        Running := Running and Index < 4 and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when others =>
        Put_Line ("An exceptiom occurred in Main_Loop.");
        raise;
end Main_Loop;
