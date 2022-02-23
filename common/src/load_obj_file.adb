
with Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Obj_File is

   procedure Parse_Face_Data
     (Face_String                                : Ada.Strings.Unbounded.Unbounded_String;
      Vertex_Indices, UV_Indices, Normal_Indices : in out Obj_Int3_Array);
   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV_Array  : in out Obj_Array2; DDS_Format : Boolean := True);
   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Array  : in out Obj_Array3);
   function Read_Index (Data  : Ada.Strings.Unbounded.Unbounded_String;
                        Start : in out Positive) return GL.Types.Int;

   --  -------------------------------------------------------------------------

   procedure Load_Data
     (File_ID                      : Ada.Text_IO.File_Type; Vertices    : in out Obj_Array3;
      UVs                          : in out Obj_Array2; Normals         : in out Obj_Array3;
      Vertex_Indicies,
      UV_Indicies, Normal_Indicies : in out Obj_Int3_Array) is
      use Ada.Strings.Unbounded;
      Line               : Unbounded_String;
      Data               : Unbounded_String;
      Label              : String (1 .. 2);
   begin
      while not End_Of_File (File_ID) loop
         Ada.Text_IO.Unbounded_IO.Get_Line (File_ID, Line);
         if Length (Line) > 1 then
            Label := To_String (Line) (1 .. 2);
            Data := Unbounded_Slice (Line, 2, To_String (Line)'Length);
            case Label (1) is
               when 'v' =>
                  case Label (2) is
                     when ' ' =>
                        Parse (Data, Vertices);
                     when 't' =>
                        Data := Unbounded_Slice (Line, 3, To_String (Line)'Length);
                        Parse (Data, UVs);
                     when 'n' =>
                        Data := Unbounded_Slice (Line, 3, To_String (Line)'Length);
                        Parse (Data, Normals);
                     when others => null;
                  end case;
               when 's' => null;
               when 'u' => null;
               when 'f' =>
                  Parse_Face_Data (Data, Vertex_Indicies, UV_Indicies,
                                   Normal_Indicies);
               when others => null;
            end case;
         end if;
      end loop;
   end Load_Data;

   --  -------------------------------------------------------------------------

   procedure Load_Object
     (File_Name                                  : String; Vertices, Normals     : out Obj_Array3;
      UVs                                        : out Obj_Array2;
      Vertex_Indices, Normal_Indices, UV_Indices : out Obj_Int3_Array) is
      Text_File_ID   : Ada.Text_IO.File_Type;
   begin
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Vertices, UVs, Normals,
                    Vertex_Indices, UV_Indices, Normal_Indices);
         Close (Text_File_ID);
      end;

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         Put_Line ("Load_Object can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Load_Object.");
         raise;
   end Load_Object;

   --  -------------------------------------------------------------------------
   --
   --      function Mesh_Size (File_Name  : String) return GL.Types.Int is
   --          use Ada.Strings.Unbounded;
   --          use GL.Types;
   --          File_ID           : Ada.Text_IO.File_Type;
   --          Mesh_Vertex_Count : Int := 0;
   --          Text              : Unbounded_String;
   --          Label             : String (1 .. 2);
   --      begin
   --          Open (File_ID, In_File, File_Name);
   --          while not End_Of_File (File_ID) loop
   --              Text := To_Unbounded_String (Get_Line (File_ID));
   --              if Length (Text) > 1 then
   --                  Label := To_String (Text) (1 .. 2);
   --                  if Label (1) = 'f' then
   --                      Mesh_Vertex_Count := Mesh_Vertex_Count + 3;
   --                  end if;
   --              end if;
   --          end loop;
   --          Close (File_ID);
   --          return Mesh_Vertex_Count;
   --
   --      exception
   --          when Ada.IO_Exceptions.Name_Error  =>
   --              --  File not found
   --              Put_Line ("Mesh_Size can't find the file " & File_Name & "!");
   --              raise;
   --          when others =>
   --              Put_Line ("An exception occurred in Mesh_Size.");
   --              raise;
   --      end Mesh_Size;

   --  -------------------------------------------------------------------------

   procedure Parse_Face_Data
     (Face_String                                : Ada.Strings.Unbounded.Unbounded_String;
      Vertex_Indices, UV_Indices, Normal_Indices : in out Obj_Int3_Array) is
      Start     : Positive := 1;
      V_Indices : GL.Types.Ints.Vector3;
      U_Indices : GL.Types.Ints.Vector3;
      N_Indices : GL.Types.Ints.Vector3;
   begin
      for index in GL.Index_3D loop
         V_Indices (index) := Read_Index (Face_String, Start);
         U_Indices (index) := Read_Index (Face_String, Start);
         N_Indices (index) := Read_Index (Face_String, Start);
      end loop;
      Vertex_Indices.Append (V_Indices);
      UV_Indices.Append (U_Indices);
      Normal_Indices.Append (N_Indices);
   end Parse_Face_Data;

   --  -------------------------------------------------------------------------

   procedure Parse (UV_String  : Ada.Strings.Unbounded.Unbounded_String;
                    UV_Array   : in out Obj_Array2;
                    DDS_Format : Boolean  := True) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      UV     : Singles.Vector2;
      Next   : Natural := 1;
      Size   : constant Natural := Length (UV_String);
      Value  : Float;
   begin
      Ada.Float_Text_IO.Get (To_String (UV_String) (Next .. Size), Value, Next);
      UV (GL.X) := Single (Value);
      Next := Next + 1;
      Ada.Float_Text_IO.Get (To_String (UV_String) (Next .. Size), Value, Next);
      UV (GL.Y) := Single (Value);
      --  Invert V coordinate since we will only use DDS texture which are inverted.
      --  Remove if you want to use TGA or BMP loaders.
      if DDS_Format then
         UV (GL.Y) := -UV (GL.Y);
      end if;
      UV_Array.Append (UV);

   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Array  : in out Obj_Array3) is
      use Ada.Strings.Unbounded;
      Vertex : GL.Types.Singles.Vector3;
      Next   : Natural := 1;
      Size   : constant Natural := Length (Vertex_String);
      Value  : float;
   begin
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Vertex (GL.X) := GL.Types.Single (Value);
      Next := Next + 1;
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Next := Next + 1;
      Vertex (GL.Y) := GL.Types.Single (Value);
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Vertex (GL.Z) := GL.Types.Single (Value);

      Vertex_Array.Append (Vertex);
   end Parse;

   --  -------------------------------------------------------------------------

   function Read_Index (Data  : Ada.Strings.Unbounded.Unbounded_String;
                        Start : in out Positive) return GL.Types.Int is
      use Ada.Strings.Unbounded;
      String_Size : constant Natural := Length (Data);
      Pos         : Positive := Start;
      Last_Pos    : Positive;
      Index       : GL.Types.Int;
      Value       : Integer;
   begin
      if Element (Data, Start) = '/' then
         Pos := Pos + 1;
      end if;

      if Element (Data, Pos) = '/' then
         Start := Pos + 1;
         Index := 0;
      else
         Ada.Integer_Text_IO.Get
           (To_String (Data) (Pos .. String_Size), Value, Last_Pos);
         Index := GL.Types.Int (Value);
         Start := Last_Pos + 1;
      end if;

      return Index;
   end Read_Index;

   --  -------------------------------------------------------------------------

end Load_Obj_File;
