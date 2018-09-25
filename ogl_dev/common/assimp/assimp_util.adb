
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

package body Assimp_Util is

   procedure Print_AI_Property_Data (Title     : String;
                                     aProperty : Material.AI_Material_Property) is
      use Ada.Containers;
      use Assimp_Types.Byte_Data_Package;
      curs : Cursor := aProperty.Data_Buffer.First;
   begin
      New_Line;
      Put_Line (Title & " Property_Data:");
      Put_Line (" Key: " & Ada.Strings.Unbounded.To_String (aProperty.Key));
      Put_Line (" Semantic, Texture_Index: " & UInt'Image (aProperty.Semantic)
                & UInt'Image (aProperty.Texture_Index));
      Put_Line (" Semantic, Data_Type, Buffer size: " &
                  Material.AI_Property_Type_Info'Image (aProperty.Data_Type) &
                  Ada.Containers.Count_Type'Image (aProperty.Data_Buffer.Length));
      if aProperty.Data_Buffer.Length > 0 then
         while Has_Element (curs) loop
            Put (Assimp_Types.C_Byte'Image (Element (curs)));
            Next (curs);
         end loop;
         New_Line;
      end if;
   end Print_AI_Property_Data;

   --  -------------------------------------------------------------------------

   procedure Print_API_Property_Data (Title     : String;
                                      aProperty : Material.API_Material_Property) is
      use Interfaces.C;
   begin
      New_Line;
      Put_Line (Title & " Property_Data:");
      Put_Line (" Key length, Key: " & size_t'Image (aProperty.Key.Length) &
                  ", " & To_String (aProperty.Key));
      if aProperty.Key.Length = 0 then
         Put_Line ("Invalid key!");
      else
         Put_Line (" Semantic, Texture_Index: " & unsigned'Image (aProperty.Semantic)
                   & unsigned'Image (aProperty.Texture_Index));
         Put_Line (" Semantic, Data_Type, Buffer size: " &
                     Material.AI_Property_Type_Info'Image (aProperty.Data_Type) &
                     unsigned'Image (aProperty.Data_Length));
      end if;
   end Print_API_Property_Data;

   --  -------------------------------------------------------------------------

--     procedure Print_API_Property_Array (Title  : String;
--                                        anArray : Material.API_Property_Array) is
--        use Interfaces.C;
--     begin
--        for index in anArray'First .. anArray'Last loop
--              Print_API_Property_Data (Title & unsigned'Image (index), anArray (index));
--        end loop;
--
--      exception
--          when others =>
--              Put_Line ("An exception occurred in Print_API_Property_Array.");
--              raise;
--     end Print_API_Property_Array;

   --  -------------------------------------------------------------------------


   function To_Assimp_API_String
     (UB_String :  Ada.Strings.Unbounded.Unbounded_String)
      return Assimp_Types.API_String is
      use Interfaces.C;
      theString     : constant String := Ada.Strings.Unbounded.To_String (UB_String);
      Assimp_String : Assimp_Types.API_String;
   begin
      Assimp_String.Length := theString'Length;
      for index in 1 ..  Assimp_String.Length loop
         Assimp_String.Data (index - 1) := To_C (theString (Integer (index)));
      end loop;
      Assimp_String.Data (Assimp_String.Length) := nul;
      return Assimp_String;
   end To_Assimp_API_String;

   --  ------------------------------------------------------------------------

   function To_OGL_Vector2 (C_Vec : API_Vectors_Matrices.API_Vector_2D)
                            return Singles.Vector2 is
      Vec : Singles.Vector2;
   begin
      Vec (GL.X) := Single (C_Vec.X);
      Vec (GL.Y) := Single (C_Vec.Y);
      return Vec;
   end To_OGL_Vector2;

   --  ------------------------------------------------------------------------

   function To_OGL_Vector3 (C_Vec : API_Vectors_Matrices.API_Vector_3D)
                            return Singles.Vector3 is
      Vec : Singles.Vector3;
   begin
      Vec (GL.X) := Single (C_Vec.X);
      Vec (GL.Y) := Single (C_Vec.Y);
      Vec (GL.Z) := Single (C_Vec.Z);
      return Vec;
   end To_OGL_Vector3;

   --  ------------------------------------------------------------------------

   function To_Colour3D (C_Colours : API_Vectors_Matrices.API_Colour_3D)
                         return Singles.Vector3 is
      theColours : Singles.Vector3;
   begin
      theColours :=
        (Single (C_Colours.R), Single (C_Colours.G), Single (C_Colours.B));
      return theColours;
   end To_Colour3D;

   --  ------------------------------------------------------------------------

   function To_Colour4D (C_Colours : API_Vectors_Matrices.API_Colour_4D)
                         return Singles.Vector4 is
      theColours : Singles.Vector4;
   begin
      theColours :=
        (Single (C_Colours.R), Single (C_Colours.G),
         Single (C_Colours.B), Single (C_Colours.A));
      return theColours;
   end To_Colour4D;

   --  ------------------------------------------------------------------------

   function To_Unbounded_String (API_String : Assimp_Types.API_String)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      use Interfaces.C.Strings;
      use Ada.Strings.Unbounded;
      API_String_Ptr : constant chars_ptr := New_Char_Array (API_String.Data);
      UB_String      :  Ada.Strings.Unbounded.Unbounded_String;
   begin
      UB_String :=
        To_Unbounded_String (Value (API_String_Ptr, API_String.Length));
      return UB_String;
   end To_Unbounded_String;

   --  ------------------------------------------------------------------------

   function To_String (API_String : Assimp_Types.API_String) return String is
      use Interfaces.C;
      String_Length : constant Integer := Integer (API_String.Length);
   begin
      if String_Length > 0 then
         declare
            theString : String (1 .. String_Length);
         begin
            for index in 1 .. String_Length loop
               theString (index) := To_Ada (API_String.Data (size_t (index - 1)));
            end loop;
            return theString;
         end;
      else
         return "";
      end if;
   end To_String;

   --  ------------------------------------------------------------------------


end Assimp_Util;
