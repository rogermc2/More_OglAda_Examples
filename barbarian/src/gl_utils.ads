
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Input_Callback;

package GL_Utils is
   use GL.Types;
   use Singles;

   package Matrix4_Package is new Ada.Containers.Doubly_Linked_Lists (Matrix4);
   type Matrix4_List is new Matrix4_Package.List with null record;

   package Singles_Package is new
     Ada.Containers.Vectors (Positive, Single);
   type Singles_List is new Singles_Package.Vector with null record;

   package Vector2_Package is new Ada.Containers.Vectors (Positive, Vector2);
   type Vector2_List is new Vector2_Package.Vector with null record;

   package Vector3_Package is new Ada.Containers.Vectors (Positive, Vector3);
   type Vector3_List is new Vector3_Package.Vector with null record;

   package Vector4_Package is new Ada.Containers.Vectors (Positive, Vector4);
   type Vector4_List is new Vector4_Package.Vector with null record;

   type Gfx_Stats is private;

   procedure Bind_VAO (VAO : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object);
   function Create_2D_VBO (Data : Vector2_Array)
                            return GL.Objects.Buffers.Buffer;
   function Create_3D_VBO (Data : Vector3_Array)
                            return GL.Objects.Buffers.Buffer;
   function Create_4D_VBO (Data : Vector4_Array)
                           return GL.Objects.Buffers.Buffer;
   function Current_Program return GL.Objects.Programs.Program;
   procedure Frame_Buffer_Resize (Window : in out Input_Callback.Barbarian_Window);
   function Is_Edit_Mode return Boolean;
   function Read_Vec2 (Vec : String) return GL.Types.Singles.Vector2;
   function Read_Vec3 (Vec : String) return GL.Types.Singles.Vector3;
   function Read_Vec4 (Vec : String) return GL.Types.Singles.Vector4;
   procedure Set_Is_Edit_Mode (Mode : Boolean);
   procedure Set_Render_Defaults;
   procedure Set_Current_Program (Current_Prog :
                                  GL.Objects.Programs.Program);
   procedure Set_Resized_View  (Bool : Boolean);
   function To_Integer (Bool : Boolean) return Integer;
   function To_String (Bool : Boolean) return String;
   function To_UB_String (Val : Integer) return
     Ada.Strings.Unbounded.Unbounded_String;
   function To_UB_String (Bool : Boolean) return
     Ada.Strings.Unbounded.Unbounded_String;
   function To_Single_Array (Vec : Singles_Package.Vector)
                              return Single_Array;
   function To_Vector2_Array (Vec : Vector2_Package.Vector)
                              return Vector2_Array;
   function To_Vector3_Array (Vec : Vector3_Package.Vector)
                              return Vector3_Array;
   function To_Vector4_Array (Vec : Vector4_Package.Vector)
                              return Vector4_Array;
   procedure Update_Batch_Count (Change : Integer);
   procedure Update_Vertex_Count (Change : Integer);
   function Verify_Bound_Framebuffer return Boolean;
   function Video_Seconds_Total return Integer;
   procedure Window_Resize (Window : in out Input_Callback.Barbarian_Window);

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
