
with Glfw;

with GL.Buffers;
with GL.Culling;
--  with GL.Objects;
with GL.Toggles;
with GL.Types.Colors;

with Utilities;

package body GL_Utils is

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
        Black : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
        begin
           Clear_Background_Colour (Black);
           Enable (Depth_Test);
           GL.Buffers.Depth_Mask (True);
           GL.Buffers.Set_Depth_Function (Less);
           Enable (Cull_Face);
           GL.Culling.Set_Cull_Face (GL.Culling.Back);
           GL.Culling.Set_Front_Face (Counter_Clockwise);
           Disable (Blend);
        end Set_Render_Defaults;

    --  ------------------------------------------------------------------------

end GL_Utils;
