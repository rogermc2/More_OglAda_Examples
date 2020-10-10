
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Buffers;
with GL.Culling;
with GL.Objects.Framebuffers;
with GL.Toggles;
with GL.Types.Colors;

with Utilities;

with Game_Utils;

package body GL_Utils is

   G_Current_Program  : GL.Objects.Programs.Program;
   Bound_VAO          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Statistics         : Gfx_Stats;
   Previous_Seconds   : Float := 0.0;

   --  ------------------------------------------------------------------------

   procedure Bind_VAO (VAO : GL.Objects.Vertex_Arrays.Vertex_Array_Object) is
   begin
      VAO.Bind;
      Bound_VAO := VAO;
   end Bind_VAO;

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

   function Current_Program return GL.Objects.Programs.Program is
   begin
      return G_Current_Program;
   end Current_Program;

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

   function Read_Vec2 (Vec : String) return GL.Types.Singles.Vector2 is
      use Ada.Strings;
      use GL.Types;
      Vec_Start : constant Integer := Vec'First;
      Vec_Last  : constant Integer := Vec'Last;
      Pos_1     : constant Natural := Fixed.Index (Vec, " ");
      theVec    : Singles.Vector2;
   begin
      theVec (GL.X) := Single'Value (Vec (Vec_Start .. Pos_1 - 1));
      theVec (GL.Y) := Single'Value (Vec (Pos_1 + 1 .. Vec_Last));
      return theVec;
   end Read_Vec2;

   --  ------------------------------------------------------------------------

   function Read_Vec3 (Vec : String) return GL.Types.Singles.Vector3 is
      use Ada.Strings;
      use GL.Types;
      Vec_Start : constant Integer := Vec'First;
      Vec_Last  : constant Integer := Vec'Last;
      Pos_1     : constant Natural := Fixed.Index (Vec, " ");
      Pos_2     : constant Natural := Fixed.Index (Vec (Pos_1 + 1 .. Vec_Last), " ");
      theVec    : Singles.Vector3;
   begin
--        Game_Utils.Game_Log ("GL_Utils Read_Vec3 Vec: " & Vec);
      theVec (GL.X) := Single'Value (Vec (Vec_Start .. Pos_1 - 1));
      theVec (GL.Y) := Single'Value (Vec (Pos_1 + 1 .. Pos_2 - 1));
      theVec (GL.Z) := Single'Value (Vec (Pos_2 + 1 .. Vec_Last));
      return theVec;
   end Read_Vec3;

   --  ------------------------------------------------------------------------

   function Read_Vec4 (Vec : String) return GL.Types.Singles.Vector4 is
      use Ada.Strings;
      use GL.Types;
      Vec_Start   : constant Integer := Vec'First;
      Vec_Last    : constant Integer := Vec'Last;
      Pos_1       : constant Natural := Fixed.Index (Vec, " ");
      Pos_2       : constant Natural := Fixed.Index (Vec (Pos_1 + 1 .. Vec_Last), " ");
      Pos_3       : constant Natural := Fixed.Index (Vec (Pos_2 + 1 .. Vec_Last), " ");
      theVec      : Singles.Vector4;
   begin
      --        Game_Utils.Game_Log ("Particle System Manager Read_Vec4 Vec: " & Vec);
      theVec (GL.X) := Single'Value (Vec (Vec_Start .. Pos_1 - 1));
      theVec (GL.Y) := Single'Value (Vec (Pos_1 + 1 .. Pos_2 - 1));
      theVec (GL.Z) := Single'Value (Vec (Pos_2 + 1 .. Pos_3 - 1));
      theVec (GL.W) := Single'Value (Vec (Pos_3 + 1 .. Vec_Last));
      return theVec;
   end Read_Vec4;

   --  ------------------------------------------------------------------------

   procedure Set_Current_Program (Current_Prog : GL.Objects.Programs.Program) is
   begin
      G_Current_Program := Current_Prog;
   end Set_Current_Program;

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

   --  ------------------------------------------------------------

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

   function Verify_Bound_Framebuffer return Boolean is
      use GL.Objects.Framebuffers;
      use Game_Utils;
      Error_Head : String := "Incomplete framebuffer, reason: ";
      FB_Status  : constant Framebuffer_Status := Status (Read_And_Draw_Target);
      OK         : Boolean := False;
   begin
      case FB_Status is
         when Complete => OK := True;
         when Undefined => Put_Line (Error_Head & "Undefined");
         when Incomplete_Attachment =>
            Put_Line (Error_Head & "Incomplete_Attachment");
         when Incomplete_Missing_Attachment =>
            Put_Line (Error_Head & "Incomplete_Missing_Attachment");
         when Incomplete_Draw_Buffer =>
            Put_Line (Error_Head & "Incomplete_Draw_Buffer");
         when Incomplete_Read_Buffer =>
            Put_Line (Error_Head & "Incomplete_Read_Buffer");
         when Unsupported => Put_Line (Error_Head & "Unsupported");
         when Incomplete_Multisample =>
            Put_Line (Error_Head & "Incomplete_Multisample");
         when  Incomplete_Layer_Targets =>
            Put_Line (Error_Head & "Incomplete_Layer_Targets");
      end case;

      return OK;

   end Verify_Bound_Framebuffer;

   --  ------------------------------------------------------------------------

end GL_Utils;
