
with Ada.Strings.Unbounded;

with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types;

package GL_Utils is

   type Gfx_Stats is private;

   procedure Bind_VAO (VAO : GL.Objects.Vertex_Arrays.Vertex_Array_Object);
   function Create_2D_VBO (Data : GL.Types.Singles.Vector2_Array)
                            return GL.Objects.Buffers.Buffer;
   function Create_3D_VBO (Data : GL.Types.Singles.Vector3_Array)
                            return GL.Objects.Buffers.Buffer;
   function Create_4D_VBO (Data : GL.Types.Singles.Vector4_Array)
                           return GL.Objects.Buffers.Buffer;
   function Current_Program return GL.Objects.Programs.Program;
   procedure Draw_Triangles (Number : GL.Types.Int);
   procedure Draw_Triangle_Strip (Number : GL.Types.Int);
   function Get_Elapsed_Seconds return Float;
   function Read_Vec2 (Vec : String) return GL.Types.Singles.Vector2;
   function Read_Vec3 (Vec : String) return GL.Types.Singles.Vector3;
   function Read_Vec4 (Vec : String) return GL.Types.Singles.Vector4;
   procedure Set_Render_Defaults;
   procedure Set_Current_Program (Current_Prog :
                                  GL.Objects.Programs.Program);
   function To_Integer (Bool : Boolean) return Integer;
   function To_String (Bool : Boolean) return String;
   function To_UB_String (Bool : Boolean) return
     Ada.Strings.Unbounded.Unbounded_String;
   procedure Update_Batch_Count (Change : Integer);
   procedure Update_Vertex_Count (Change : Integer);
   function Verify_Bound_Framebuffer return Boolean;

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
