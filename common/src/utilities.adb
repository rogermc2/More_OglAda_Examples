
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with GL.Buffers;
with GL.Context;
with GL.Objects.Shaders.Lists;

package body Utilities is

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased GL.Types.Single;
   procedure Print_Singles_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Singles_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (GL.Types.Single'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Singles_Vector;

   --  -------------------------------------------------------------------

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased GL.Types.Int;
   procedure Print_Int_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Int_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (GL.Types.Int'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Int_Vector;

   --  -------------------------------------------------------------------

   procedure Print_Ints_Vector2 is
     new Print_Int_Vector (GL.Index_2D, GL.Types.Ints.Vector2);
   procedure Print_Singles_Vector2 is
     new Print_Singles_Vector (GL.Index_2D, GL.Types.Singles.Vector2);
   procedure Print_Ints_Vector3 is
     new Print_Int_Vector (GL.Index_3D, GL.Types.Ints.Vector3);
   procedure Print_Singles_Vector3 is
     new Print_Singles_Vector (GL.Index_3D, GL.Types.Singles.Vector3);
   procedure Print_Singles_Vector4 is
     new Print_Singles_Vector (GL.Index_Homogeneous, GL.Types.Singles.Vector4);
   procedure Print_Singles_Vector5 is
     new Print_Singles_Vector (Maths.Index_5, Maths.Vector5);
   procedure Print_Singles_Vector6 is
     new Print_Singles_Vector (Maths.Index_6, Maths.Vector6);
   procedure Print_Singles_Vector8 is
     new Print_Singles_Vector (Maths.Index_8, Maths.Vector8);

   --  ---------------------------------------------------------------

   --  Set_Color_Clear_Value sets the value to which a buffer should be set
   --  when cleared.
   --  Clear "clears" selected values to the previously selected value set by
   --  Set_Color_Clear_Value .
   procedure Clear_All (Colour : GL.Types.Colors.Color) is
   begin
      GL.Buffers.Set_Color_Clear_Value (Colour);
      GL.Buffers.Clear ((True, True, True, True));
   end Clear_All;

   --  ------------------------------------------------------------------------

   procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color) is
   begin
      GL.Buffers.Set_Color_Clear_Value (Colour);
      GL.Buffers.Clear ((False, False, False, True));
   end Clear_Background_Colour;

   --  ------------------------------------------------------------------------

   procedure Clear_Background_Colour (Colour : GL.Types.Colors.Basic_Color) is
      use GL.Types.Colors;
   begin
      GL.Buffers.Set_Color_Clear_Value ((Colour (R), Colour (G), Colour (B), 0.0));
      GL.Buffers.Clear ((False, False, False, True));
   end Clear_Background_Colour;

   --  ------------------------------------------------------------------------

   procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color) is
   begin
      GL.Buffers.Set_Color_Clear_Value (Colour);
      GL.Buffers.Clear ((True, False, False, True));
   end Clear_Background_Colour_And_Depth;

   --  ------------------------------------------------------------------------

   procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Basic_Color) is
      use GL.Types.Colors;
   begin
      GL.Buffers.Set_Color_Clear_Value ((Colour (R), Colour (G), Colour (B), 0.0));
      GL.Buffers.Clear ((True, False, False, True));
   end Clear_Background_Colour_And_Depth;

   --  ------------------------------------------------------------------------

   procedure Clear_Colour is
   begin
      GL.Buffers.Clear ((True, False, False, False));
   end Clear_Colour;

   --  ------------------------------------------------------------------------

   procedure Clear_Colour_Buffer_And_Depth is
   begin
      GL.Buffers.Clear ((True, False, False, True));
   end Clear_Colour_Buffer_And_Depth;

   --  ------------------------------------------------------------------------

   procedure Clear_Depth is
   begin
      GL.Buffers.Clear ((True, False, False, False));
   end Clear_Depth;

   --  ------------------------------------------------------------------------

   procedure Enable_Mouse_Callbacks (Window : in out Glfw.Windows.Window; Enable : Boolean) is
   begin
      if Enable then
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Enter);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
      else
         Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
         Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Enter);
         Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
         Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
      end if;
   end Enable_Mouse_Callbacks;

   --  ------------------------------------------------------------------------

   function Flatten (anArray : Ints_Vector4_Array4) return GL.Types.Int_Array is
      use GL.Types;
      Out_Size        : constant Int := 16 * anArray'Length;
      Flattened_Array : Int_Array (1 .. Out_Size);
      Vec4_Array      : Ints.Vector4_Array  (1 .. 4);
      Vec4            : Ints.Vector4;
      Out_Index       : Int := 0;
   begin
      for Index in anArray'First .. anArray'Last loop
         Vec4_Array := anArray (Index);
         for Inner_Index in Int range 1 .. 4 loop
            Vec4 := Vec4_Array (Inner_Index);
            for Inner2_Index in GL.Index_Homogeneous'First .. GL.Index_Homogeneous'Last loop
               Out_Index := Out_Index + 1;
               Flattened_Array (Out_Index) := Vec4 (Inner2_Index);
            end loop;
         end loop;
      end loop;
      return Flattened_Array;
   end Flatten;

   --  ------------------------------------------------------------------------

   procedure Print_Array6 (Name : String; anArray : Maths.Vector6_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector6 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_Array6;

   --  -------------------------------------------------------------------------

   procedure Print_Byte_Array (Name          : String; anArray : Byte_Array;
                               Start, Finish : GL.Types.UInt) is
      use GL.Types;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Int (Start) >= anArray'First and then Int (Finish) <= anArray'Last then
         for Index in Start .. Finish loop
            Put (UInt'Image (Index) & ": " &
                   Byte'Image (anArray (Int (Index))) & "   ");
            Count := Count + 1;
            if Count > 5 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Byte_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_Byte_Array;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array2 (Name : String; anArray : GL.Types.Singles.Vector2_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector2 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array2;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array2 (Name : String; anArray : GL.Types.Ints.Vector2_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Ints_Vector2 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array2;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Ints.Vector3_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Ints_Vector3 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array3;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Singles.Vector3_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector3 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array3;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Singles.Vector4_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector4 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array4;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Array8 (Name : String; anArray : Maths.Vector8_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector8 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array8;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Int_Array (Name : String; anArray : GL.Types.Int_Array) is
      use GL.Types;
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put_Line (Int'Image (Index) & ":  " & Int'Image (anArray (Index)));
      end loop;
      New_Line;
   end Print_GL_Int_Array;

   --  ------------------------------------------------------------------------

   procedure Print_GL_Int_Array (Name          : String; anArray : GL.Types.Int_Array;
                                 Start, Finish : GL.Types.Int) is
      use GL.Types;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Finish loop
            Put (Int'Image (Index) & ": " & Int'Image (anArray (Index)) &
                   "   ");
            Count := Count + 1;
            if Count > 3 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Utilities.Print_GL_Int_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_GL_Int_Array;

   --  ------------------------------------------------------------------------

   procedure Print_GL_UInt_Array (Name : String; anArray : GL.Types.UInt_Array) is
      use GL.Types;
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put_Line (Int'Image (Index) & ":  " & UInt'Image (anArray (Index)));
      end loop;
      New_Line;
   end Print_GL_UInt_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix (Name    : String;
                           aMatrix : GL.Types.Ints.Matrix4) is
      use GL.Types;
   begin
      Put_Line (Name & ":");
      for Row in  aMatrix'Range loop
         for Column in aMatrix'Range (2) loop
            Put (Int'Image (aMatrix (Row, Column)) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix (Name    : String;
                           aMatrix : GL.Types.Singles.Matrix3) is
   begin
      Put_Line (Name & ":");
      for Row in GL.Index_3D'Range loop
         for Column in GL.Index_3D'Range loop
            Put (GL.Types.Single'Image (aMatrix (Row, Column)) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix (Name    : String;
                           aMatrix : GL.Types.Singles.Matrix4) is
   begin
      Put_Line (Name & ":");
      for Row in GL.Index_Homogeneous'Range loop
         for Column in GL.Index_Homogeneous'Range loop
            Put (GL.Types.Single'Image (aMatrix (Row, Column)) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Singles_Array (Name          : String; anArray : Singles_Array;
                                  Start, Finish : GL.Types.Int) is
      use GL.Types;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Finish loop
            Put (Int'Image (Index) & ": " & Single'Image (anArray (Index)) &
                   "   ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Utilities.Print_Singles_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_Singles_Array;

   --  ------------------------------------------------------------------------

   procedure Print_UByte_Array (Name          : String; anArray : UByte_Array;
                                Start, Finish : GL.Types.UInt) is
      use GL.Types;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Int (Start) >= anArray'First and then Int (Finish) <= anArray'Last then
         for Index in Start .. Finish loop
            Put (UInt'Image (Index) & ": " &
                   UByte'Image (anArray (Int (Index))) & "   ");
            Count := Count + 1;
            if Count > 5 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Byte_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_UByte_Array;

   --  ------------------------------------------------------------------------


   procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector2) is
   begin
      Print_Singles_Vector2 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Vector (Name : String; aVector : GL.Types.Ints.Vector3) is
   begin
      Print_Ints_Vector3 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3) is
   begin
      Print_Singles_Vector3 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4) is
   begin
      Print_Singles_Vector4 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Vector (Name : String; aVector : Maths.Vector5) is
   begin
      Print_Singles_Vector5 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Vector (Name : String; aVector : Maths.Vector8) is
   begin
      Print_Singles_Vector8 (Name, aVector);
   end Print_Vector;

   --  ------------------------------------------------------------------------

   procedure Set_Uniform_Location (Shader_Program : GL.Objects.Programs.Program;
                                   Location       : String;
                                   theUniform     : in out GL.Uniforms.Uniform) is
      use GL.Uniforms;
   begin
      theUniform := GL.Objects.Programs.Uniform_Location (Shader_Program, Location);
      if theUniform < 0 then
         Put_Line ("Set_Uniform_Location, Uniform " & Location & " not found.");
      end if;
   end Set_Uniform_Location;

   --  -------------------------------------------------------------------------

   procedure Show_GL_Data is
      GL_Version                : Unbounded_String;
      Renderer                  : Unbounded_String;
      Shading_Language_Version  : Unbounded_String;
   begin
      GL_Version := To_Unbounded_String (GL.Types.Int'Image (GL.Context.Major_Version) & "." &
                                         GL.Types.Int'Image (GL.Context.Minor_Version));
      Renderer := To_Unbounded_String (GL.Context.Renderer);
      Shading_Language_Version :=
        To_Unbounded_String (GL.Context.Primary_Shading_Language_Version);
      New_Line;
      Put_Line ("OpenGL version supported: " & To_String (GL_Version));
      Put_Line ("Renderer: " & To_String (Renderer));
      Put_Line ("Primary_Shading_Language_Version: " & To_String (Shading_Language_Version));
      New_Line;
   end Show_GL_Data;

   --  ------------------------------------------------------------------------

   procedure Show_Shader_Program_Data (aProgram : GL.Objects.Programs.Program) is
      use GL.Objects;
      Shaders_List        : constant Shaders.Lists.List :=
                              Programs.Attached_Shaders (aProgram);
      List_Cursor         : Shaders.Lists.Cursor := Shaders_List.First;
      Shader1             : constant Shaders.Shader :=
                              Shaders.Lists.Element (List_Cursor);
      Shader_Count        : Positive := 1;
   begin
      Put_Line ("Shader: " & Positive'Image (Shader_Count));
      Put_Line (Shaders.Source (Shader1));

      while Shaders.Lists.Has_Next (List_Cursor)  loop
         Put_Line (" Show_Shader_Program_Data loop entry");
         List_Cursor := Shaders.Lists.Next (List_Cursor);
         declare
            ShaderN  : constant Shaders.Shader :=
                         Shaders.Lists.Element (List_Cursor);
         begin
            Shader_Count := Shader_Count + 1;
            Put_Line ("Shader: " & Positive'Image (Shader_Count));
            Put_Line (Shaders.Source (ShaderN));
            New_Line;
         end;
      end loop;
      New_Line;

   exception
      when others =>
         Put_Line ("An exception occurred in Show_Shader_Program_Data.");
         raise;
   end Show_Shader_Program_Data;

   --  ------------------------------------------------------------------------

   procedure Show_Shader_Info_Log (aProgram : GL.Objects.Programs.Program) is
      use GL.Objects;
      Shaders_List        : constant Shaders.Lists.List :=
                              Programs.Attached_Shaders (aProgram);
      List_Cursor         : Shaders.Lists.Cursor := Shaders_List.First;
      Shader_Count        : Positive := 1;
   begin
      Put_Line ("Shader: " & Positive'Image (Shader_Count) & " log:");
      while Shaders.Lists.Has_Next (List_Cursor)  loop
         List_Cursor := Shaders.Lists.Next (List_Cursor);
         declare
            ShaderN  : constant Shaders.Shader :=
                         Shaders.Lists.Element (List_Cursor);
         begin
            Shader_Count := Shader_Count + 1;
            Put_Line ("Shader: " & Positive'Image (Shader_Count) & " log:");
            declare
               Shader_Log : constant String :=
                              GL.Objects.Shaders.Info_Log (ShaderN);
            begin
               Put_Line (Shader_Log);
            end;
         end;
      end loop;
      New_Line;
   exception
      when others =>
         Put_Line ("An exception occurred in Show_Shader_Info_Log.");
         raise;
   end Show_Shader_Info_Log;

   --  ------------------------------------------------------------------------

end Utilities;
