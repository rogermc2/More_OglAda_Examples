
package GL_Utils is

    type Gfx_Stats is private;

    function Get_Elapsed_Seconds return Float;
    procedure Set_Render_Defaults;

private
     type Gfx_Stats is record
        Batch_Count             : Integer := 0; --  num of draws per frame
	Vertex_Count            : Integer := 0; --  total num of vertices drawn per frame
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
