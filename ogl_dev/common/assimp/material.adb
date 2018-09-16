
with System;

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;
with Material_System;

package body Material is

   subtype Data_Size is Interfaces.c.unsigned range 0 .. 1024;  --  To avoid possible storage error
   type Byte_Data_Array is array (Data_Size range <>) of UByte;
   pragma Convention (C, Byte_Data_Array);

   type Data_Record (length : Data_Size := 1) is record
      Bytes : aliased Byte_Data_Array (1 .. length);
   end record;

   type Data_Record_Access is access all Data_Record;

   type Property_Data_Array is array (UInt range <>) of aliased Data_Record;

   type Material_Property is record
      Key           : Assimp_Types.API_String;
      Semantic      : Interfaces.C.unsigned := 0;
      Texture_Index : Interfaces.C.unsigned := 0;
      Data_Length   : Interfaces.C.unsigned := 0;  --  Actual must not be 0
      --  Data_Type provides information for the property.
      --  It defines the data layout inside the data buffer.
      --  This is used by the library internally to perform debug checks and to
      --  utilize proper type conversions.
      Data_Type     : AI_Property_Type_Info := PTI_Float;
      --  Data holds the property's value. Its size is always Data_Length
      Data          : Data_Record_Access;
      --        Data          : access Byte_Data_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Material_Property);

   type Material_Property_Array is array (Interfaces.C.unsigned range <>) of
     aliased Material_Property;
   pragma Convention (C, Material_Property_Array);


   function To_AI_Property_List (anAPI_Material     : API_Material;
                                 Property_Ptr_Array : API_Property_Ptr_Array)
                                 return AI_Material_Property_List;
   procedure To_API_Property (aProperty       : AI_Material_Property;
                              Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
                              theAPI_Property : in out API_Material_Property);
   procedure To_API_Material (aMaterial       : AI_Material;
                              theAPI_Material : in out API_Material);

   procedure To_Material_Property_Array (Properties     : AI_Material_Property_List;
                                         Property_Data  : in out Property_Data_Array;
                                         Property_Array : in out Material_Property_Array);

   --  -------------------------------------------------------------------------

   procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                          Tex_Index : UInt := 0;
                          Path      : out Ada.Strings.Unbounded.Unbounded_String;
                          Result    : out Assimp_Types.API_Return) is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use Assimp_Types;

      --  Material_Property_Array_Pointer must be declared here to prevent non-local error messages
      Material_Property_Default : Material_Property :=
                                    ((0, (others => Interfaces.C.char'Val (0))),
                                     0, 0, 0, PTI_Float, null);
      package Material_Property_Array_Package is new Interfaces.C.Pointers
        (Interfaces.C.unsigned, Material_Property, Material_Property_Array,
         Material_Property_Default);
      subtype Material_Property_Array_Pointer is Material_Property_Array_Package.Pointer;

      type Property_Ptr_Array is array (Interfaces.C.unsigned range <>) of
        aliased Material_Property_Array_Pointer;
      pragma Convention (C, Property_Ptr_Array);

      --  Pointer to a list (array) of all material properties loaded:
      --  C_STRUCT aiMaterialProperty** mProperties;
      --  unsigned int mNumProperties; Number of properties in the data base
      --  unsigned int mNumAllocated;  Storage allocated
      type API_Material_Tex is record
         Properties     : access Material_Property_Array_Pointer := null;
         Num_Properties : Interfaces.C.unsigned := 0;
         Num_Allocated  : Interfaces.C.unsigned := 0;
      end record;
      pragma Convention (C_Pass_By_Copy, API_Material_Tex);

      function Get_Material_String (aMaterial : access API_Material_Tex;
                                    Tex_Type  : unsigned;
                                    Index : unsigned)
                                           return Assimp_Types.API_String;
      pragma Import (C, Get_Material_String, "aiGetMaterialString");

      function Get_Material_Texture_Count (aMaterial : access API_Material_Tex;
                                           Tex_Type  : unsigned)
                                           return unsigned;
      pragma Import (C, Get_Material_Texture_Count, "aiGetMaterialTextureCount");

      --  aiGetMaterialTexture(this,type,index,path,mapping,uvindex,blend,op,mapmode
      function API_Get_Material_Texture (aMaterial : access API_Material_Tex;
                                         Tex_Type  : AI_Texture_Type;
                                         Index     : Interfaces.C.unsigned;
                                         Path      : access Assimp_Types.API_String := null;
                                         Mapping   : access AI_Texture_Mapping := null;
                                         UV_Index  : access Interfaces.C.unsigned := null;
                                         Blend     : access Interfaces.C.C_float := null;
                                         Op        : access AI_Texture_Op := null;
                                         Map_Mode  : access AI_Texture_Map_Mode := null)
                                         return Assimp_Types.API_Return;
      pragma Import (C, API_Get_Material_Texture, "aiGetMaterialTexture");

      Material           : aliased API_Material_Tex;
      --  Path returned by aiGetMaterialTexture to texture file
      Assimp_Path        : aliased Assimp_Types.API_String;
      Properties_Length  : unsigned := unsigned (Length (aMaterial.Properties));
      API_Property_Array : Material_Property_Array (1 .. Properties_Length);
      Property_Data      : Property_Data_Array (1 .. Uint (aMaterial.Properties.Length));
      API_Prop_Ptr_Array : aliased Property_Ptr_Array (1 .. Properties_Length);
      Prop_Ptr_Array_Ptr : access Material_Property_Array_Pointer;
      Texture_Count      : unsigned := 0;
      Material_String    : Assimp_Types.API_String;
   begin
      Material.Num_Properties := unsigned (aMaterial.Properties.Length);
      Material.Num_Allocated := unsigned (aMaterial.Num_Allocated);
      --  Generate an array of Material_Property records for
      --  the elements of Property_Ptr_Array to point to
      To_Material_Property_Array (aMaterial.Properties, Property_Data,
                                  API_Property_Array);

      for index in 1 .. Properties_Length loop
         API_Property_Array (index).Data := Property_Data (UInt (index))'unchecked_Access;
         API_Prop_Ptr_Array (index) := API_Property_Array (index)'Access;
      end loop;

      Prop_Ptr_Array_Ptr := API_Prop_Ptr_Array (1)'Access;
      Material.Properties := Prop_Ptr_Array_Ptr;
      --  Material.Properties -> Prop_Ptr_Array_Ptr -> API_Property_Array (1)
      Put_Line ("Material.Get_Texture, API_Property_Array (1).Data_Type: " &
                  AI_Property_Type_Info'Image (API_Property_Array (1).Data_Type));
      Put_Line ("Material.Get_Texture, API_Property_Array (2).Data_Type: " &
                  AI_Property_Type_Info'Image (API_Property_Array (2).Data_Type));
      Put_Line ("Material.Get_Texture, API_Property_Array (5).Data_Type: " &
                  AI_Property_Type_Info'Image (API_Property_Array (5).Data_Type));
      Put_Line ("Material.Get_Texture, Prop_Ptr_Array_Ptr -> API_Property_Array (1).Data_Type: " &
                  AI_Property_Type_Info'Image (Prop_Ptr_Array_Ptr.all.Data_Type));
      Put_Line ("Material.Get_Texture, Material.Properties -> Prop_Ptr_Array_Ptr -> API_Property_Array (1).Data_Type: "
                & AI_Property_Type_Info'Image (Material.Properties.all.Data_Type));
      declare
         theData   : Byte_Data_Array (1 .. Material.Properties.all.Data_Length) :=
                       Material.Properties.all.Data.Bytes;
         theString : String (1 .. Integer (Material.Properties.all.Data_Length));
      begin
         for index in 1 .. Material.Properties.all.Data_Length loop
            Put (UByte'Image (theData (index)) & " ");
            theString (Integer (index)) := character'Val (theData (index));
         end loop;
         New_Line;
         Put_Line ("Material.Get_Texture, Material.Properties -> Prop_Ptr_Array_Ptr -> API_Property_Array (1).Data: "
                   & theString);
      end;

      for index in 1 .. 16#c# loop
         Texture_Count := Get_Material_Texture_Count
           (Material'Access, Interfaces.C.unsigned (index));
         if Texture_Count > 0 then
            Material_String := Get_Material_String
              (Material'Access, Interfaces.C.unsigned (index), 1);
         end if;
         Put_Line ("Material.Get_Texture, Texture_Count" & Integer'Image (index)
                   & unsigned'Image (Texture_Count));
      end loop;

      Result :=
        API_Get_Material_Texture
          (Material'Access, AI_Texture_Diffuse, 0, Assimp_Path'Access);
      --          API_Get_Material_Texture
      --            (Material'Access, Tex_Type, unsigned (Tex_Index), Assimp_Path'Access);
      if Result = API_Return_Success then
         Path := To_Unbounded_String (To_Ada (Assimp_Path.Data));
      elsif Result = API_Return_Out_Of_Memory then
         Put_Line ("Material.Get_Texture, aiGetMaterialTexture returned out of memory error.");
      else
         Put_Line ("Material.Get_Texture, aiGetMaterialTexture failed.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.Get_Texture.");
         raise;
   end Get_Texture;

   --  -------------------------------------------------------------------------

   function Get_Texture_Count (aMaterial : AI_Material;
                               Tex_Type  : AI_Texture_Type) return GL.Types.UInt is
      use AI_Material_Property_Package;
      Props     : constant AI_Material_Property_List := aMaterial.Properties;
      aProperty : AI_Material_Property;
      Curs      : Cursor := Props.First;
      Count     : GL.Types.UInt := 0;
   begin
      while Has_Element (Curs) loop
         aProperty := Element (Curs);
         if Ada.Strings.Unbounded.To_String (aProperty.Key) = "$tex.file" then
            Count := Count + 1;
         end if;
         Next (Curs);
      end loop;
      return Count;
   end Get_Texture_Count;

   --  -------------------------------------------------------------------------

   function To_AI_Material (C_Material : API_Material) return AI_Material is
      use Interfaces.C;
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
            theProperties_Ptr_Array := Value (C_Material.Properties, ptrdiff_t (Num_Property));
            --           Put_Line ("Material.To_AI_Material Key.Length: " &
            --                       size_t'Image (theProperties_Ptr_Array (2).Key.Length) &
            --                       "  " & To_Ada (theProperties_Ptr_Array (2).Key.Data));
            --           Put_Line ("Material.To_AI_Material Data_Length: " &
            --                       unsigned'Image (theProperties_Ptr_Array (2).Data_Length));
            --           Put_Line ("Material.To_AI_Material Data_Type: " &
            --                       AI_Property_Type_Info'Image (theProperties_Ptr_Array (2).Data_Type));
            --              Put_Line ("Material.To_AI_Material Index: " &
            --                          unsigned'Image (theProperties_Ptr_Array (5).Texture_Index));
            theMaterial.Properties := To_AI_Property_List
              (C_Material, theProperties_Ptr_Array);
            theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
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
                                 C_Material_Array : API_Material_Array)
                                 return AI_Material_Map is
      use Interfaces.C;
      Material_Map : AI_Material_Map;
      aMaterial    : AI_Material;
   begin
      Put_Line ("Material.To_AI_Materials_Map Num_Materials: " &
                  Interfaces.C.unsigned'Image (Num_Materials));
      for mat in 1 .. Num_Materials loop
         aMaterial := To_AI_Material (C_Material_Array (mat));
         Material_Map.Insert (UInt (mat), aMaterial);
      end loop;
      return Material_Map;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Materials_Map.");
         raise;
   end To_AI_Materials_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Property (anAPI_Material : API_Material;
                            API_Property   : API_Material_Property)
                            return AI_Material_Property is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Assimp_Types;
      API_Prop     : API_Material_Property := API_Property;
      Key_Length   : constant size_t := API_Prop.Key.Length;
      Data_Length  : constant size_t := size_t (API_Prop.Data_Length);
      Data_String  : Assimp_Types.API_String;
      Raw_Data     : AI_Material_Property_List;
      Result       : Assimp_Types.API_Return := Assimp_Types.API_Return_Failure;
      AI_Property  : AI_Material_Property;
   begin
      --        Put_Line ("Material.To_AI_Property Key_Length: " & size_t'Image (Key_Length));
      if Key_Length > 0 then
         declare
            Key_Data  : constant String (1 .. Integer (Key_Length)) := To_Ada (API_Prop.Key.Data);
         begin
            --              Put_Line ("Material.To_AI_Property Key_Data: " & Key_Data);
            AI_Property.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
         end;
      end if;

      AI_Property.Semantic := UInt (API_Prop.Semantic);
      AI_Property.Texture_Index := UInt (API_Prop.Texture_Index);
      AI_Property.Data_Type := API_Prop.Data_Type;
      --        Put_Line ("Material.To_AI_Property Semantic, Texture Index: " &
      --                    UInt'Image (AI_Property.Semantic) & UInt'Image (AI_Property.Texture_Index));

      if Data_Length > 0  then
         --           Put_Line ("Material.To_AI_Property Data_Type: " &
         --                       AI_Property_Type_Info'Image (API_Property.Data_Type));
         Result := Material_System.Get_Material_String (anAPI_Material, API_Property.Key,
                                                        API_Property.Data_Type,
                                                        API_Property.Texture_Index, Data_String);
         --           Put_Line ("Material.To_AI_Property Result: " & API_Return'Image (Result));
         if Result = API_RETURN_SUCCESS then
            --              Put_Line ("Material.To_AI_Property Data_Length: " & size_t'Image (Data_Length));
            for index in 1 .. Data_Length loop
               AI_Property.Data_Buffer.Append (API_Prop.Data.all);
               Raw_Data_Pointers.Increment (API_Prop.Data);  --  Data is access Assimp_Types.Raw_Byte_Data;
            end loop;
         end if;
      else
         Put_Line ("Material.To_AI_Property detected illegal Data_Length.");
      end if;
      --        Put_Line ("Material.To_AI_Property exit Semantic, Texture Index: " &
      --                    UInt'Image (AI_Property.Semantic) &
      --                    UInt'Image (AI_Property.Texture_Index));
      return AI_Property;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property.");
         raise;

   end To_AI_Property;

   --  ----------------------------------------------------------------------

   function To_AI_Property_List (anAPI_Material     : API_Material;
                                 Property_Ptr_Array : API_Property_Ptr_Array)
                                 return AI_Material_Property_List is
      use Interfaces.C;
      aProperty      : API_Material_Property;
      AI_Properties  : AI_Material_Property_List;
      AI_Property    : AI_Material_Property;
   begin
      for Property_Index in unsigned range 1 .. Property_Ptr_Array'Length loop
         --           New_Line;
         aProperty := Property_Ptr_Array (Property_Index).all;
         --           Put_Line ("Material.To_AI_Property_List appending Property_Index " &
         --                       unsigned'Image (Property_Index));
         --           Put_Line ("Material.To_AI_Property_List Key.Length: " &
         --                       size_t'Image (aProperty.Key.Length) &
         --                       "  " & To_Ada (aProperty.Key.Data));
         --           Put_Line ("Material.To_AI_Property_List Data_Length: " &
         --                       unsigned'Image (aProperty.Data_Length));
         --           Put_Line ("Material.To_AI_Property_List Data_Type: " &
         --                       AI_Property_Type_Info'Image (aProperty.Data_Type));
         --           Put_Line ("Material.To_AI_Property_List Texture_Index: " &
         --                       unsigned'Image (Property_Index) & ": " &
         --                       unsigned'Image (aProperty.Texture_Index));
         AI_Property := To_AI_Property (anAPI_Material, aProperty);
         AI_Properties.Append (AI_Property);
         --           New_Line;
      end loop;
      return AI_Properties;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property_List.");
         raise;

   end To_AI_Property_List;

   --  ----------------------------------------------------------------------

   procedure To_API_Material (aMaterial       : AI_Material;
                              theAPI_Material : in out API_Material) is
      use Interfaces.C;
      use AI_Material_Property_Package;
      Property_Curs       : Cursor := aMaterial.Properties.First;
      Ptr_Array           : API_Property_Ptr_Array
        (1 .. unsigned (Length (aMaterial.Properties)));
      Index               : unsigned := 0;
      aProperty           : AI_Material_Property;
      API_Property_Record : API_Material_Property;
   begin
      --        Properties     : Property_Ptr_Array_Package.Pointer := null;
      while Has_Element (Property_Curs) loop
         Index := Index + 1;
         aProperty := Element (Property_Curs);
         declare
            --  Raw_Data holds the property's value.
            Raw_Data  : Assimp_Types.Raw_Byte_Data
              (1 .. UInt (aProperty.Data_Buffer.Length));
            --              Raw_Ptr   : Assimp_Types.Data_Pointer := Raw_Data (1)'Access;
         begin
            To_API_Property (aProperty, Raw_Data, API_Property_Record);
            Ptr_Array (index) := null;
            --           theAPI_Material.Properties := API_Property_Record;
         end;
         Next (Property_Curs);
      end loop;

      theAPI_Material.Num_Properties := unsigned (Length (aMaterial.Properties));
      theAPI_Material.Num_Allocated := unsigned (aMaterial.Num_Allocated);
   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_API_Material.");
         raise;

   end To_API_Material;

   --  ----------------------------------------------------------------------

   procedure To_API_Property (aProperty       : AI_Material_Property;
                              Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
                              theAPI_Property : in out API_Material_Property) is
      use Interfaces.C;
      use Assimp_Types;
      use Assimp_Types.Byte_Data_Package;
      Name       : String := Ada.Strings.Unbounded.To_String (aProperty.Key);
      Data       : Byte_Data_List := aProperty.Data_Buffer;
      Curs       : Cursor := Data.First;
      Raw_Length : unsigned := unsigned (aProperty.Data_Buffer.Length);
      Index      : UInt := 0;
   begin
      theAPI_Property.Key.Length := Name'Length;
      theAPI_Property.Key.Data := To_C (Name);
      theAPI_Property.Semantic := unsigned (aProperty.Semantic);
      theAPI_Property.Texture_Index := unsigned (aProperty.Texture_Index);
      theAPI_Property.Data_Length := Raw_Length;
      theAPI_Property.Data_Type := aProperty.Data_Type;
      --  Raw_Data holds the property's value. Its size is always Data_Length
      while Has_Element (Curs) loop
         Index := Index + 1;
         Raw_Data (Index) := Element (Curs);
         Next (Curs);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_API_Property.");
         raise;
   end To_API_Property;

   --  ----------------------------------------------------------------------

   procedure To_Material_Property_Array (Properties     : AI_Material_Property_List;
                                         Property_Data  : in out Property_Data_Array;
                                         Property_Array : in out Material_Property_Array) is
      use Interfaces.C;
      use AI_Material_Property_Package;
      use Assimp_Types.Byte_Data_Package;
      Property_Cursor      : AI_Material_Property_Package.Cursor :=
                               Properties.First;
      Data_Cursor          : Assimp_Types.Byte_Data_Package.Cursor;
      Data_Length          : unsigned := 0;
      aProperty            : AI_Material_Property;
      Index                : unsigned := 0;
   begin
      while Has_Element (Property_Cursor) loop
         Index := Index + 1;
         aProperty := Element (Property_Cursor);
         Data_Cursor := aProperty.Data_Buffer.First;
         Property_Array (index).Key := Assimp_Util.To_Assimp_API_String (aProperty.Key);
         Property_Array (index).Semantic := unsigned (aProperty.Semantic);
         Property_Array (index).Texture_Index :=
           unsigned (aProperty.Texture_Index);
         Data_Length := unsigned (aProperty.Data_Buffer.Length);
         Property_Array (index).Data_Length := Data_Length;
         Property_Array (index).Data_Type := aProperty.Data_Type;
         declare
            Data       : Byte_Data_Array (1 .. Data_Length);
            Data_Rec   : Data_Record (Data_Length);
            Data_Index : unsigned := 0;
         begin
            while Has_Element (Data_Cursor) loop
               Data_Index := Data_Index + 1;
               Data (Data_Index) := Element (Data_Cursor);
               Next (Data_Cursor);
            end loop;
            Data_Rec.Bytes := Data;
            Property_Data (UInt (index)) := Data_Rec;
         end;
         Next (Property_Cursor);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_Material_Property_Array.");
         raise;
   end To_Material_Property_Array;

   --  ----------------------------------------------------------------------

end Material;
