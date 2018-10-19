
with Interfaces.C.Strings;

with Ada.Containers;
with Ada.Unchecked_Conversion;

with GL.Types;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;
with Assimp_Types;
with Material_Keys;
with Utilities;

with Material_Keys;

package body AI_Conversion is

   type String_4 is new String (1 .. 4);
   type Byte_Array4 is array (1 .. 4) of GL.Types.UByte;
   type Byte_Array12 is array (1 .. 12) of GL.Types.UByte;

   procedure To_AI_Property_List (anAPI_Material     : Material.API_Material;
                                  Property_Ptr_Array : Material.API_Property_Ptr_Array;
                                  AI_Properties      : out Material.AI_Material_Property_List);
   function To_Integer (Bytes_In : Byte_Array4) return GL.Types.Int;

   --  ----------------------------------------------------------------------

   function Data_To_UB_String (String_Data : Assimp_Types.Raw_Byte_Data) return
     Ada.Strings.Unbounded.Unbounded_String is
      use Interfaces;
      use GL.Types;

      Data_Length : UInt := String_Data'Length;
      Data_String : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.To_Unbounded_String ("");
      Str_Data    : String (1 .. Integer (Data_Length - 4));
      aByte       : UByte;
   begin
      if Data_Length >= 5 then
         for index in 5 .. Data_Length - 1 loop
            aByte := String_Data (index);
            Str_Data (Integer (index - 4)) := Character'Val (Unsigned_16 (aByte));
         end loop;
         Data_String := Ada.Strings.Unbounded.To_Unbounded_String (Str_Data);
      end if;
      Put_Line ("Material_System.Data_To_UB_String Data_String: " &
                  Ada.Strings.Unbounded.To_String (Data_String));
      return Data_String;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Data_To_UB_String.");
         raise;
   end Data_To_UB_String;

   --  -------------------------------------------------------------------------

   function To_AI_Material (C_Material : Material.API_Material) return Material.AI_Material is
      use Interfaces.C;
      use Material;
      Num_Property  : constant unsigned := C_Material.Num_Properties;
      theMaterial   : AI_Material;
   begin
      Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                  unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
      if Num_Property > 0 then
         declare
            use Property_Ptr_Array_Package;
            theProperties_Ptr_Array : API_Property_Ptr_Array (1 .. Num_Property);
         begin
            theProperties_Ptr_Array := Property_Ptr_Array_Package.Value
              (C_Material.Properties, ptrdiff_t (Num_Property));
            To_AI_Property_List
              (C_Material, theProperties_Ptr_Array, theMaterial.Properties);
            theMaterial.Num_Allocated := GL.Types.UInt (C_Material.Num_Allocated);
         end;
      end if;
      return theMaterial;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Material.");
         raise;
   end To_AI_Material;

   --  ------------------------------------------------------------------------

   function To_AI_Materials_Map (Num_Materials    : Interfaces.C.unsigned := 0;
                                 C_Material_Array : in out Material.API_Material_Array)
                                  return Material.AI_Material_Map is
      use Interfaces.C;
      Material_Map : Material.AI_Material_Map;
      aMaterial    : Material.AI_Material;
   begin
      Put_Line ("AI_Conversion.To_AI_Materials_Map Num_Materials: " &
                  Interfaces.C.unsigned'Image (Num_Materials));
      for mat in 1 .. Num_Materials loop
         aMaterial := To_AI_Material (C_Material_Array (mat));
         Material_Map.Insert (GL.Types.UInt (mat - 1), aMaterial);
      end loop;
      return Material_Map;

   exception
      when others =>
         Put_Line ("An exception occurred in AI_Conversion.To_AI_Materials_Map.");
         raise;
   end To_AI_Materials_Map;

   --  ------------------------------------------------------------------------

   procedure To_AI_Property (anAPI_Material : Material.API_Material;
                             API_Property   : Material.API_Material_Property;
                             theAI_Property : out Material.AI_Material_Property) is
      use Interfaces.C;
      use GL.Types;
      use Assimp_Types;
      use Raw_Data_Pointers;

      Key_Length    : size_t := API_Property.Key.Length;
      --  Data_Length number of bytes
      Data_Length   : constant UInt := UInt (API_Property.Data_Length);
      Data_Array    : Raw_Byte_Data (1 .. Data_Length);
      Data4         : Byte_Array4;
      Key_String    : constant String (1 .. Integer (Key_Length)) :=
                        To_Ada (API_Property.Key.Data);
      AI_Property   : Material.AI_Material_Property (API_Property.Data_Type);
   begin
      if Key_Length > 0 then
         declare
            Key_Data  : constant String (1 .. Integer (Key_Length)) :=
                          To_Ada (API_Property.Key.Data);
         begin
            AI_Property.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
         end;

         AI_Property.Semantic := UInt (API_Property.Semantic);
         AI_Property.Texture_Index := UInt (API_Property.Texture_Index);
         AI_Property.Data_Length := Data_Length;
         Assimp_Util.Print_API_Property_Data ("AI_Conversion.To_AI_Property API", API_Property);
         if AI_Property.Data_Length > 0 and API_Property.Data_Ptr /= null then
            Data_Array := Raw_Data_Pointers.Value (API_Property.Data_Ptr, ptrdiff_t (Data_Length));
            case API_Property.Data_Type is
               when Material.PTI_Float => AI_Property.Data_Buffer.Float_Data := 0.0;
               when Material.PTI_Double => AI_Property.Data_Buffer.Double_Data := 0.0;
               when Material.PTI_String => AI_Property.Data_Buffer.String_Data :=
                      Data_To_UB_String (Data_Array);
               when Material.PTI_Integer =>
                  for index in 1 .. 4 loop
                     Data4 (index) := Data_Array (UInt (index));
                  end loop;
                  AI_Property.Data_Buffer.Integer_Data := To_Integer (Data4);
               when Material.PTI_Buffer => AI_Property.Data_Buffer.Buffer_Data := 0;
               when others => null;
            end case;
         end if;
      else
         Put_Line ("AI_Conversion.To_AI_Property, invalid key detected.");
      end if;
      theAI_Property := AI_Property;
         Assimp_Util.Print_AI_Property_Data ("AI_Conversion.To_AI_Property AI",
                                             theAI_Property);

   exception
      when others =>
         Put_Line ("An exception occurred in AI_Conversion.To_AI_Property.");
         raise;

   end To_AI_Property;

   --  ----------------------------------------------------------------------

   procedure To_AI_Property_List (anAPI_Material     : Material.API_Material;
                                  Property_Ptr_Array : Material.API_Property_Ptr_Array;
                                  AI_Properties      : out Material.AI_Material_Property_List) is
      use Interfaces.C;
      use Material;
      aProperty      : API_Material_Property;
      AI_Property    : AI_Material_Property;
   begin
      Put_Line ("Material.To_AI_Property_List, Property_Ptr_Array'Length" &
                  unsigned'Image (Property_Ptr_Array'Length));
      for Property_Index in unsigned range 1 .. Property_Ptr_Array'Length loop
         aProperty := Property_Ptr_Array (Property_Index).all;
         New_Line;
         Put_Line ("Material.To_AI_Property_List, setting AI Property" &
                     unsigned'Image (Property_Index));
         To_AI_Property (anAPI_Material, aProperty, AI_Property);
         AI_Properties.Append (AI_Property);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property_List.");
         raise;
   end To_AI_Property_List;

   --  ----------------------------------------------------------------------

   --     procedure To_API_Material (aMaterial       : AI_Material;
   --                                theAPI_Material : in out API_Material) is
   --        use Interfaces.C;
   --        use AI_Material_Property_Package;
   --        Property_Curs       : Cursor := aMaterial.Properties.First;
   --        Property_Array           : API_Property_Array
   --          (1 .. unsigned (Length (aMaterial.Properties)));
   --        Index               : unsigned := 0;
   --        aProperty           : AI_Material_Property;
   --        API_Property_Record : API_Material_Property;
   --     begin
   --        while Has_Element (Property_Curs) loop
   --           Index := Index + 1;
   --           aProperty := Element (Property_Curs);
   --           declare
   --              --  Raw_Data holds the property's value.
   --              Raw_Data  : Assimp_Types.Raw_Byte_Data
   --                (1 .. UInt (aProperty.Data_Buffer.Length));
   --              --              Raw_Ptr   : Assimp_Types.Data_Pointer := Raw_Data (1)'Access;
   --           begin
   --              To_API_Property (aProperty, Raw_Data, API_Property_Record);
   --              --           theAPI_Material.Properties := API_Property_Record;
   --           end;
   --           Next (Property_Curs);
   --        end loop;
   --
   --        theAPI_Material.Num_Properties := unsigned (Length (aMaterial.Properties));
   --        theAPI_Material.Num_Allocated := unsigned (aMaterial.Num_Allocated);
   --     exception
   --        when others =>
   --           Put_Line ("An exception occurred in AI_Conversion.To_API_Material.");
   --           raise;
   --
   --     end To_API_Material;

   --  ----------------------------------------------------------------------

   --     procedure To_API_Property (aProperty       : Material.AI_Material_Property;
   --                                Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
   --                                theAPI_Property : in out Material.API_Material_Property) is
   --        use Interfaces.C;
   --        use GL.Types;
   --        use Assimp_Types;
   --        use Assimp_Types.Byte_Data_Package;
   --        Name       : String := Ada.Strings.Unbounded.To_String (aProperty.Key);
   --        Data       : Byte_Data_List := aProperty.Data_Buffer;
   --        Curs       : Cursor := Data.First;
   --        Raw_Length : unsigned := unsigned (aProperty.Data_Buffer.Length);
   --        Index      : UInt := 0;
   --     begin
   --        theAPI_Property.Key.Length := Name'Length;
   --        theAPI_Property.Key.Data := To_C (Name);
   --        theAPI_Property.Semantic := unsigned (aProperty.Semantic);
   --        theAPI_Property.Texture_Index := unsigned (aProperty.Texture_Index);
   --        theAPI_Property.Data_Length := Raw_Length;
   --        theAPI_Property.Data_Type := aProperty.Data_Type;
   --        --  Raw_Data holds the property's value. Its size is always Data_Length
   --        while Has_Element (Curs) loop
   --           Index := Index + 1;
   --           Raw_Data (Index) := Element (Curs);
   --           Next (Curs);
   --        end loop;
   --
   --     exception
   --        when others =>
   --           Put_Line ("An exception occurred in AI_Conversion.To_API_Property.");
   --           raise;
   --     end To_API_Property;

   --  ----------------------------------------------------------------------

   function To_Integer (Bytes_In : Byte_Array4) return GL.Types.Int is
      use Interfaces;
      Int32  : Unsigned_32 := 0;
      Int8   : Unsigned_8;
   begin
      for index in 0 .. 3  loop
         Int8 := Unsigned_8 (Bytes_In (4 - index));
         Int32 := Int32 + Unsigned_32 (Int8);
      end loop;
      return GL.Types.Int (Int32);

   end To_Integer;

   --  -------------------------------------------------------------------------

end AI_Conversion;
