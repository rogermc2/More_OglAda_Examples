
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Data                  : constant GL.Types.Single_Array (1 .. 5) :=
                             (1.0, 2.0, 3.0, 4.0, 5.0);

    Rendering_Program     : GL.Objects.Programs.Program;
    Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Transform_Feedback_Buffer : GL.Objects.Buffers.Buffer;
    Input_Attribute      : GL.Attributes.Attribute;

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
--          use Maths.Single_Math_Functions;
--          Now  : constant Glfw.Seconds := Glfw.Time;
    begin
        Rendering_Program.Use_Program;
--          GL.Uniforms.Set_Single (Colour_Location,
--                               (1.0 + Sin (4.0 * Single (Now))) / 2.0, 0.0, 0.0);
        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 3);
    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup is
        use GL.Objects.Buffers;
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use GL.Types;
      use Program_Loader;
      Varyings  : constant String := "outValue";
      OK        : Boolean;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Rendering_Program := Program_From
          ((Src ("src/shaders/feedback_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/feedback_geometry_shader.glsl", Geometry_Shader)));

      Rendering_Program.Use_Program;
      --  Interleaved_Attribs means that the varyings are recorded
      --  consecetively into a single buffer.
      Transform_Feedback_Varyings (Rendering_Program, Varyings,
                                   Interleaved_Attribs);
     Rendering_Program.Link;
      OK := GL.Objects.Programs.Link_Status (Rendering_Program);
      if not OK then
         Put_Line ("Main_Loop.Setup, Rendering_Program Link for Varyings failed");
         Put_Line (GL.Objects.Programs.Info_Log (Rendering_Program));
      end if;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Singles_Buffer (Array_Buffer, Data, Static_Draw);

        Rendering_Program.Use_Program;
        Input_Attribute := GL.Objects.Programs.Attrib_Location
          (Rendering_Program, "inValue");
        GL.Attributes.Enable_Vertex_Attrib_Array (Input_Attribute);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Input_Attribute, 1, GL.Types.Single_Type, False, 0, 0);

      Transform_Feedback_Buffer.Initialize_Id;
      Array_Buffer.Bind (Transform_Feedback_Buffer);
      Array_Buffer.Allocate (3 * Data'Size / 8, Static_Read);

        GL.Uniforms.Set_Single (Colour_Location, 1.0, 0.0, 0.0);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup;
    while Running loop
        Render;
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
