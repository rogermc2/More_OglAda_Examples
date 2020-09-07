
with Glfw;

with GL.Buffers;
with GL.Culling;
--  with GL.Objects;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Utilities;

package body GL_Utils is

    Statistics       : Gfx_Stats;
    Previous_Seconds : Float := 0.0;

    --  ------------------------------------------------------------------------

--      function Create_VBO (Dimensionality, Length : Int;
--                           theBuffer : out GL.Objects.Buffers.Buffer)
--                           return GL.Types.UInt is
--          use GL.Objects.Buffers;
--      begin
--          theBuffer.Initialize_Id;
--         Array_Buffer.Bind (theBuffer);
--          Utilities.Load_Vertex_Buffer (
--          return GL.Objects.Raw_Id (GL.Objects.GL_Object (theBuffer));
--      end Create_VBO;

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
