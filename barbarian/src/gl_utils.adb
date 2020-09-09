
with Glfw;

with GL.Buffers;
with GL.Culling;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Utilities;

package body GL_Utils is

    Statistics       : Gfx_Stats;
    Previous_Seconds : Float := 0.0;

    --  ------------------------------------------------------------------------

    function Create_2D_VBO (Data : GL.Types.Singles.Vector2_Array)
                            return GL.Objects.Buffers.Buffer is
        use GL.Objects.Buffers;
        New_Buffer : Buffer;
    begin
        New_Buffer.Initialize_Id;
        Array_Buffer.Bind (New_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Data, Static_Draw);
        return New_Buffer;
    end Create_2D_VBO;

    --  ------------------------------------------------------------------------

    function Create_3D_VBO (Data : GL.Types.Singles.Vector3_Array)
                            return GL.Objects.Buffers.Buffer is
        use GL.Objects.Buffers;
        New_Buffer : Buffer;
    begin
        New_Buffer.Initialize_Id;
        Array_Buffer.Bind (New_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Data, Static_Draw);
        return New_Buffer;
    end Create_3D_VBO;

    --  ------------------------------------------------------------------------

    procedure Draw_Triangles (Number : GL.Types.Int) is
        use GL.Types;
    begin
         GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, Number);
         Statistics.Vertex_Count := Statistics.Vertex_Count + Number;
         Statistics.Batch_Count := Statistics.Batch_Count + 1;
    end Draw_Triangles;

    --  ------------------------------------------------------------------------

    procedure Draw_Triangle_Strip (Number : GL.Types.Int) is
        use GL.Types;
    begin
         GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangle_Strip, 0, Number);
         Statistics.Vertex_Count := Statistics.Vertex_Count + Number;
         Statistics.Batch_Count := Statistics.Batch_Count + 1;
    end Draw_Triangle_Strip;

    --  -----------------------------------------------------------------------

    function Get_Elapsed_Seconds return float is
        Elapsed : constant Float := Float (Glfw.Time) - Previous_Seconds;
    begin
        Previous_Seconds := Float (Glfw.Time);
        return Elapsed;
    end Get_Elapsed_Seconds;

    --  ------------------------------------------------------------------------

    procedure Set_Render_Defaults is
        use GL.Toggles;
        use Utilities;
        Black : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
        begin
           Clear_Background_Colour (Black);
           Enable (Depth_Test);
           GL.Buffers.Depth_Mask (True);
           GL.Buffers.Set_Depth_Function (GL.Types.Less);
           Enable (Cull_Face);
           GL.Culling.Set_Cull_Face (GL.Culling.Back);
           GL.Culling.Set_Front_Face (GL.Types.Counter_Clockwise);
           Disable (Blend);
        end Set_Render_Defaults;

    --  ------------------------------------------------------------------------

    procedure Update_Batch_Count (Change : Integer) is
    begin
        Statistics.Batch_Count := Statistics.Batch_Count + Change;
    end Update_Batch_Count;

    --  ------------------------------------------------------------------------

    procedure Update_Vertex_Count (Change : Integer) is
        use GL.Types;
    begin
        Statistics.Vertex_Count := Statistics.Vertex_Count + Int (Change);
    end Update_Vertex_Count;

    --  ------------------------------------------------------------------------

end GL_Utils;
