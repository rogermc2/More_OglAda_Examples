
with GL.Types;

package GL_Utils is

    type Gfx_Stats is private;

    procedure Draw_Triangles (Number : GL.Types.Int);
    procedure Draw_Triangle_Strip (Number : GL.Types.Int);
    function Get_Elapsed_Seconds return Float;
    procedure Set_Render_Defaults;
    procedure Update_Batch_Count (Change : Integer);
    procedure Update_Vertex_Count (Change : Integer);

private
    type Gfx_Stats is record
        Batch_Count             : Natural := 0; --  num of draws per frame
        Vertex_Count            : GL.Types.Int := 0; --  total num of vertices drawn per frame
        Uniform_Count           : Integer := 0; --  num of uniform calls per frame
        Prog_Change_Count       : Integer := 0; --  num of shader changes per frame
        Prog_Change_Avoid_Count : Integer := 0; --  num of shader changes avoided per frame
        Tex_Bind_Avoid_Count    : Integer := 0; --  num of texture binds avoided per frame
        --  min/max/avg frame times to put in log to help with debugging
        Min_Frame_CPU_Ms        : Float := 0.0;
        Max_Frame_CPU_Ms        : Float := 0.0;
        Avg_Frame_CPU_Ms        : Float := 0.0;
        Current_Frame_CPU_Ms    : Float := 0.0;
    end record;

end GL_Utils;