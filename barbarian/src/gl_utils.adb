
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with GL.Buffers;
with GL.Culling;
with GL.Objects.Framebuffers;
with GL.Toggles;
with GL.Types.Colors;

with Utilities;

with Camera;
with Game_Utils;
with Settings;

package body GL_Utils is

   G_Current_Program   : GL.Objects.Programs.Program;
   Bound_VAO           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Statistics          : Gfx_Stats;
   Previous_Seconds    : Float := 0.0;
   Total_Video_Seconds : constant Integer := 10;
   G_Resized_View      : Boolean := False;
   Edit_Mode           : Boolean := False;

   --  ------------------------------------------------------------------------

   procedure Bind_VAO (VAO : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object) is
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

   function Create_4D_VBO (Data : GL.Types.Singles.Vector4_Array)
                           return GL.Objects.Buffers.Buffer is
      use GL.Objects.Buffers;
      New_Buffer : Buffer;
   begin
      New_Buffer.Initialize_Id;
      Array_Buffer.Bind (New_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Data, Static_Draw);
      return New_Buffer;
   end Create_4D_VBO;

   --  ------------------------------------------------------------------------

   function Current_Program return GL.Objects.Programs.Program is
   begin
      return G_Current_Program;
   end Current_Program;

   --  ------------------------------------------------------------------------

   procedure Frame_Buffer_Resize
     (Window : in out Input_Callback.Barbarian_Window) is
      use GL.Types;
      use Camera;
      Width  : Glfw.Size;
      Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Width, Height);
--        Game_Utils.Game_Log ("framebuffer resized to " & Glfw.Size'Image (Width)
--                             & " x " & Glfw.Size'Image (Height));
--        Put_Line ("framebuffer resized to " & Glfw.Size'Image (Width)
--                  & " x " & Glfw.Size'Image (Height));
      Settings.Set_Framebuffer_Width (Int (Width));
      Settings.Set_Framebuffer_Height (Int (Height));
      if Integer (Width) * Integer (Height) > 1920 * 1080 and
        Settings.Super_Sample_Anti_Aliasing > 1.0 then
        Put_Line ("Very high-resolution mode detected. Forcing SSAA to 1.0x");
	Game_Utils.Game_Log ("very high-res mode detected. Forcing SSAA to 1.0x");
	Settings.Set_SSAA (1.0);
      end if;
      Recalculate_Perspective (Field_Of_View_Y, Single (Width), Single (Height),
                               Near, Far);
      G_Resized_View := True;
   end Frame_Buffer_Resize;

   --  ------------------------------------------------------------------------

   function Is_Edit_Mode return Boolean is
   begin
       return Edit_Mode;
   end Is_Edit_Mode;

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

   procedure Set_Is_Edit_Mode (Mode : Boolean) is
   begin
       Edit_Mode := Mode;
   end Set_Is_Edit_Mode;

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

   procedure Set_Resized_View  (Bool : Boolean) is
   begin
      G_Resized_View := Bool;
   end Set_Resized_View;

   --  ------------------------------------------------------------

   function To_Integer (Bool : Boolean) return Integer is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Integer;

   --  ------------------------------------------------------------------------

   function To_Single_Array (Vec : Singles_Package.Vector)
                              return Single_Array is
      use GL.Types;
      use Singles_Package;
      Curs          : Cursor := Vec.First;
      Singles_Array : Single_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Singles_Array'Range loop
         Singles_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Singles_Array;

   end To_Single_Array;

   --  ------------------------------------------------------------------------

   function To_String (Bool : Boolean) return String is
   begin
      if Bool then
         return ("1");
      else
         return ("0");
      end if;
   end To_String;

   --  ------------------------------------------------------------------------

   function To_UB_String (Val : Integer) return
     Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
   begin
      return To_Unbounded_String (Integer'Image (Val));
   end To_UB_String;

   --  ------------------------------------------------------------------------

   function To_UB_String (Bool : Boolean)
                          return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
   begin
      if Bool then
         return To_Unbounded_String ("1");
      else
         return To_Unbounded_String ("0");
      end if;
   end To_UB_String;

   --  ------------------------------------------------------------------------

   function To_Vector2_Array (Vec : Vector2_Package.Vector)
                              return Vector2_Array is
      use GL.Types;
      use Vector2_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Vector2_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector2_Array;

   --  ------------------------------------------------------------------------

   function To_Vector3_Array (Vec : Vector3_Package.Vector)
                              return Vector3_Array is
      use GL.Types;
      use Vector3_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Vector3_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector3_Array;

   --  ------------------------------------------------------------------------

   function To_Vector4_Array (Vec : Vector4_Package.Vector)
                              return Vector4_Array is
      use GL.Types;
      use Vector4_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Vector4_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector4_Array;

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

   function Verify_Bound_Framebuffer return Boolean is
      use GL.Objects.Framebuffers;
      use Game_Utils;
      Error_Head : constant String := "Incomplete framebuffer, reason: ";
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

   function Video_Seconds_Total return Integer is
   begin
      return Total_Video_Seconds;
   end Video_Seconds_Total;

   --  ------------------------------------------------------------------------

   procedure Window_Resize (Window : in out Input_Callback.Barbarian_Window) is
      use GL.Types;
      use Camera;
      Width  : Glfw.Size;
      Height : Glfw.Size;
   begin
     Window.Get_Size (Width, Height);
--        Game_Utils.Game_Log ("framebuffer resized to " & Glfw.Size'Image (Width)
--                             & " x " & Glfw.Size'Image (Height));
--        Put_Line ("framebuffer resized to " & Glfw.Size'Image (Width)
--                  & " x " & Glfw.Size'Image (Height));
      Settings.Set_Window_Width (Integer (Width));
      Settings.Set_Window_Height (Integer (Height));

   end Window_Resize;

   --  ------------------------------------------------------------------------

end GL_Utils;
