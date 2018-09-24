
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;
with Material_System;

package body Material is

   type C_Byte  is new Interfaces.C.char;
   subtype Data_Size is Interfaces.C.unsigned range 0 .. 1024;  --  To avoid possible storage error
   type Byte_Data_Array is array (Data_Size range <>) of aliased C_Byte;
   pragma Convention (C, Byte_Data_Array);

   package Byte_Array_Package is new Interfaces.C.Pointers
     (Data_Size, UByte, Byte_Data_Array, UByte'Last);
   subtype Byte_Array_Pointer is Byte_Array_Package.Pointer;

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
      --        Data_Ptr      : Byte_Array_Pointer := null;
      Data_Ptr      : Interfaces.C.Strings.chars_ptr :=
                        Interfaces.C.Strings.Null_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Material_Property);

   type Material_Property_Array is array (Interfaces.C.unsigned range <>) of
     aliased Material_Property;
   pragma Convention (C, Material_Property_Array);

   function To_AI_Property_List (anAPI_Material : in out API_Material;
                                 Property_Array : API_Property_Array;
                                 AI_Properties  : out AI_Material_Property_List)
                                 return Assimp_Types.API_Return;
   procedure To_API_Property (aProperty       : AI_Material_Property;
                              Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
                              theAPI_Property : in out API_Material_Property);
   procedure To_API_Material (aMaterial       : AI_Material;
                              theAPI_Material : in out API_Material);

   procedure Load_API_Property_Array (Properties     : AI_Material_Property_List;
                                      Property_Array : in out Material_Property_Array);

   --  -------------------------------------------------------------------------

   procedure Get_Texture_Via_C (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                          Tex_Index : UInt := 0;
                          Path      : out Ada.Strings.Unbounded.Unbounded_String;
                          Result    : out Assimp_Types.API_Return) is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use Assimp_Types;

      --  What counts for accessibility is the place where the type of an object
      --  is declared not where the object itself is declared.

      --  A char_array_access may be unneeded as  char_array is meant to be
      --  used where C expects char * .
      --  Just pass To_C ("from") right to C.

      --  Material_Property_Array_Pointer must be declared here to prevent non-local error messages
      Material_Property_Default : Material_Property :=
                                    ((0, (others => Interfaces.C.char'Val (0))),
                                     0, 0, 0, PTI_Float, Interfaces.C.Strings.Null_Ptr);

      package Material_Property_Array_Package is new Interfaces.C.Pointers
        (Interfaces.C.unsigned, Material_Property, Material_Property_Array,
         Material_Property_Default);
      subtype Material_Property_Array_Pointer is Material_Property_Array_Package.Pointer;

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

      --        function Get_Material_String (aMaterial : access API_Material_Tex;
      --                                      Tex_Type  : unsigned;
      --                                      Index : unsigned)
      --                                             return Assimp_Types.API_String;
      --        pragma Import (C, Get_Material_String, "aiGetMaterialString");
      --
      --        function Get_Material_Texture_Count (aMaterial : access API_Material_Tex;
      --                                             Tex_Type  : unsigned)
      --                                             return unsigned;
      --        pragma Import (C, Get_Material_Texture_Count, "aiGetMaterialTextureCount");

      --  aiGetMaterialTexture(this,type,index,path,mapping,uvindex,blend,op,mapmode
      function API_Get_Material_Texture (aMaterial : access API_Material_Tex;
                                         Tex_Type  : AI_Texture_Type;
                                         Index     : Interfaces.C.unsigned;
                                         Path      : access Assimp_Types.API_String := null;
                                         Mapping   : access AI_Texture_Mapping := null;
                                         UV_Index  : access Interfaces.C.unsigned := null;
                                         Blend     : access Interfaces.C.C_float := null;
                                         Op        : access AI_Texture_Op := null;
                                         Map_Mode  : access AI_Texture_Map_Mode := null;
                                         Flags     : access Interfaces.C.unsigned := null)
                                         return Assimp_Types.API_Return;
      pragma Import (C, API_Get_Material_Texture, "aiGetMaterialTexture");

      Material_Tex         : aliased API_Material_Tex;
      --  Path returned by aiGetMaterialTexture to texture file
      Assimp_Path          : aliased Assimp_Types.API_String;
      Properties_Length    : unsigned := unsigned (Length (aMaterial.Properties));
      API_Property_Array   : Material_Property_Array (1 .. Properties_Length);
      Property_Array_Ptr   : aliased Material_Property_Array_Pointer;
      Texture_Count        : unsigned := 0;
      Returned_Path        : access Assimp_Types.API_String := null;
      API_Test_Prop_Array  : Material_Property_Array (1 .. Properties_Length);
      API_Test_Prop_Ptr    : Material_Property_Array_Pointer;
   begin

     Assimp_Util.Print_AI_Property_Data ("Material.Get_Texture Property 1",
                                          aMaterial.Properties.First_Element);
      Material_Tex.Num_Properties := unsigned (aMaterial.Properties.Length);
      Material_Tex.Num_Allocated := unsigned (aMaterial.Num_Allocated);
      --  Generate an array of Material_Property records for
      --  the elements of Property_Ptr_Array to point to
      Load_API_Property_Array (aMaterial.Properties, API_Property_Array);

      Property_Array_Ptr := API_Property_Array (1)'Access;
      Material_Tex.Properties := Property_Array_Ptr'Access;
      Put_Line ("Material.Get_Texture, Material_Tex.Properties set");

      --  Properties printout
      API_Test_Prop_Ptr := Material_Tex.Properties.all;
      API_Test_Prop_Array := Material_Property_Array_Package.Value
        (API_Test_Prop_Ptr, ptrdiff_t (Properties_Length));
      Put_Line ("Material.Get_Texture, Material_Tex.Properties Key: " &
                  size_t'Image (API_Test_Prop_Array (1).Key.Length) & "  " &
                  Assimp_Util.To_String (API_Test_Prop_Array (1).Key));
      Put_Line ("Material.Get_Texture, Material_Tex.Properties Data Length, Type: " &
                  unsigned'Image (API_Test_Prop_Array (1).Data_Length) & "  " &
                  AI_Property_Type_Info'Image (API_Test_Prop_Array (1).Data_Type));
      --  End Properties printout

      New_Line;
      Result := API_Get_Material_Texture
        (Material_Tex'Access, Tex_Type, unsigned (Tex_Index), Assimp_Path'Access);
      Put_Line ("Material.Get_Texture, returned from API_Get_Material_Texture");
      if Result = API_Return_Success then
         Put ("Material.Get_Texture, Assimp_Path characters: ");
         for index in 1 .. Assimp_Path.Length loop
            Assimp_Path.Data (index) := Assimp_Path.Data (index);
            Put (To_Ada (Assimp_Path.Data (index)));
         end loop;
         New_Line;
         Path := Ada.Strings.Unbounded.To_Unbounded_String
           (Assimp_Util.To_String (Assimp_Path));
         Put_Line ("Material.Get_Texture Assimp_Path: " & size_t'Image (Assimp_Path.Length) &
                     "  " &  Assimp_Util.To_String (Assimp_Path));
      elsif Result = API_Return_Out_Of_Memory then
         Put_Line ("Material.Get_Texture, aiGetMaterialTexture returned out of memory error.");
      else
         Put_Line ("Material.Get_Texture, aiGetMaterialTexture failed.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.Get_Texture_Via_C.");
         raise;
   end Get_Texture_Via_C;

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

   procedure Load_API_Property_Array (Properties     : AI_Material_Property_List;
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
         --           Assimp_Util.Print_AI_Property_Data ("Material.Load_API_Property_Array", aProperty);
         --           New_Line;
         Property_Array (index).Key := Assimp_Util.To_Assimp_API_String (aProperty.Key);
         Property_Array (index).Semantic := unsigned (aProperty.Semantic);
         Property_Array (index).Texture_Index :=
           unsigned (aProperty.Texture_Index);
         Data_Length := unsigned (aProperty.Data_Buffer.Length);
         Property_Array (index).Data_Length := Data_Length;
         Property_Array (index).Data_Type := aProperty.Data_Type;
         declare
            Data       : char_array (1 .. size_t (Data_Length));
            Data_Index : size_t := 0;
            aChar      : Character;
         begin
            while Has_Element (Data_Cursor) loop
               Data_Index := Data_Index + 1;
               aChar := To_Ada (char (Element (Data_Cursor)));
               Data (Data_Index) := To_C (aChar);
               Next (Data_Cursor);
            end loop;

            Property_Array (index).Data_Ptr := Strings.New_Char_Array (Data);
         end;
         Next (Property_Cursor);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.Load_API_Property_Array.");
         raise;
   end Load_API_Property_Array;

   --  ----------------------------------------------------------------------

   function To_AI_Material (C_Material : in out API_Material) return AI_Material is
      use Interfaces.C;
      Num_Property  : constant unsigned := C_Material.Num_Properties;
      theMaterial   : AI_Material;
      Result        : Assimp_Types.API_Return := Assimp_Types.API_Return_Failure;
   begin
      Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                  unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
      if Num_Property > 0 then
         declare
            --              use Property_Ptr_Array_Package;
            use API_Property_Array_Package;
            Property_Array_Ptr  : API_Material_Property_Ptr := C_Material.Properties.all;
            theProperties_Array : API_Property_Array (1 .. Num_Property);
            anAPI_Property      : API_Material_Property;
         begin
            theProperties_Array := API_Property_Array_Package.Value (Property_Array_Ptr, ptrdiff_t (Num_Property));
            anAPI_Property := theProperties_Array (1);
            Result := To_AI_Property_List
              (C_Material, theProperties_Array, theMaterial.Properties);
            theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
            Assimp_Util.Print_AI_Property_Data ("Material.To_AI_Material Property 1",
                                                theMaterial.Properties.First_Element);
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
                                 C_Material_Array : in out API_Material_Array)
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

   function To_AI_Property (anAPI_Material : in out API_Material;
                            API_Property   : API_Material_Property;
                            theAI_Property : out AI_Material_Property)
                            return Assimp_Types.API_Return is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Ada.Strings.Unbounded;
      use Assimp_Types;
      use API_Property_Array_Package;

      Num_Properties    : constant unsigned := anAPI_Material.Num_Properties;
      L_API_Material    : aliased API_Material := anAPI_Material;
      Prop_Access       : access API_Material_Property_Ptr;
      Prop_Ptr          : API_Material_Property_Ptr;
      API_Prop_Array    : API_Property_Array (1 .. anAPI_Material.Num_Properties);
      API_Prop          : API_Material_Property := API_Property;
      Key_Length        : size_t := API_Prop.Key.Length;
      Data_Length       : constant size_t := size_t (API_Prop.Data_Length);
      Integer_Data      : aliased Interfaces.C.int := 0;
      Float_Data        : aliased C_float := 0.0;
      String_Data       : aliased Assimp_Types.API_String;
      Test_Property     : aliased API_Material_Property;
      Test_Property_Ptr : aliased access API_Material_Property :=
                            Test_Property'Access;
      Key_String        : constant String (1 .. Integer (Key_Length)) :=
                            To_Ada (API_Property.Key.Data);
      Key_Data_Ptr      : constant chars_ptr := New_String (Key_String);
      Result            : Assimp_Types.API_Return := Assimp_Types.API_Return_Failure;
   begin
      New_Line;
      Assimp_Util.Print_API_Property_Data ("Material.To_AI_Property API", API_Property);
      if anAPI_Material.Properties = null then
            Put_Line ("Material.To_AI_Property anAPI_Material.Properties is null");
      else
            Prop_Access := anAPI_Material.Properties;
            Prop_Ptr := Prop_Access.all;
            if Prop_Ptr = Null then
                Put_Line ("Material.To_AI_Property Prop_Ptr is null");
            else
                API_Prop_Array := API_Property_Array_Package.Value (Prop_Ptr, ptrdiff_t (Num_Properties));
--                  Assimp_Util.Print_API_Property_Array ("Material.To_AI_Property API_Prop_Array",
--                                                        API_Prop_Array);
            end if;
       end if;

--        Result := Material_System.Get_Material_Property
--          (L_API_Material'Access, Key_Data_Ptr, API_Property.Data_Type,
--           API_Property.Texture_Index, Test_Property_Ptr'Access);
--
--        if Result /= API_RETURN_SUCCESS then
--           Put_Line ("Material.To_AI_Property Get_Material_Property failed:");
--        else
--           Put_Line ("Material.To_AI_Property Get_Material_Property succeeded:");

         if Key_Length > 0 then
            declare
               Key_Data  : constant String (1 .. Integer (Key_Length)) := To_Ada (API_Prop.Key.Data);
            begin
               theAI_Property.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
            end;
         end if;

         theAI_Property.Semantic := UInt (API_Prop.Semantic);
         theAI_Property.Texture_Index := UInt (API_Prop.Texture_Index);
         theAI_Property.Data_Type := API_Prop.Data_Type;
         New_Line;
         Assimp_Util.Print_AI_Property_Data ("Material.To_AI_Property AI_Property",
                                             theAI_Property);

         if Data_Length > 0  then
            case theAI_Property.Data_Type is
            when PTI_String => Put_Line ("Material.To_AI_Property PTI_String.");
--                 Result := Material_System.Get_Material_String
--                   (L_API_Material'Access, Key_Data_Ptr, PTI_String,
--                    API_Property.Texture_Index, String_Data'Access);
--                 if Result = API_RETURN_SUCCESS then
--                    Put_Line ("Material.To_AI_Property Data_Length and Data_String: " &
--                                size_t'Image (Data_Length) &
--                                Assimp_Util.To_String (String_Data));
--                    for index in 1 .. Data_Length loop
--                       theAI_Property.Data_Buffer.Append (API_Prop.Data_Ptr.all);
--                       Raw_Data_Pointers.Increment (API_Prop.Data_Ptr);  --  Data is access Assimp_Types.Raw_Byte_Data;
--                    end loop;
--                 else
--                    Put_Line ("Material.To_AI_Property Get_Material_String failed.");
--                 end if;

            when PTI_Buffer => Put_Line ("Material.To_AI_Property PTI_Buffer.");
            when PTI_Double => Put_Line ("Material.To_AI_Property PTI_Double.");
            when PTI_Float => Put_Line ("Material.To_AI_Property PTI_Float.");
--                 Result := Material_System.Get_Material_Float
--                   (L_API_Material'Access, Key_Data_Ptr, PTI_Float,
--                    API_Property.Texture_Index, Float_Data'Access);
--                 if Result = API_RETURN_SUCCESS then
--                    Put_Line ("Material.To_AI_Property Float_Data: " &
--                                C_float'Image (Float_Data));
--                 else
--                    Put_Line ("Material.To_AI_Property Get_Material_Float failed.");
--                 end if;
            when PTI_Integer => Put_Line ("Material.To_AI_Property PTI_Integer.");
--                 Result := Material_System.Get_Material_Integer
--                   (L_API_Material'Access, Key_Data_Ptr, PTI_Integer,
--                    API_Property.Texture_Index, Integer_Data'Access);
--                 if Result = API_RETURN_SUCCESS then
--                    Put_Line ("Material.To_AI_Property Integer_Data: " &
--                                Interfaces.C.int'Image (Integer_Data));
--                 else
--                    Put_Line ("Material.To_AI_Property Get_Material_Integer failed.");
--                 end if;
            when PTI_Force32Bit => Put_Line ("Material.To_AI_Property PTI_Force32Bit.");
            end case;
         else
            Put_Line ("Material.To_AI_Property detected illegal Data_Length.");
         end if;
--        end if;  --  API_RETURN_SUCCESS
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property.");
         raise;

   end To_AI_Property;

   --  ----------------------------------------------------------------------

   function To_AI_Property_List (anAPI_Material : in out API_Material;
                                 Property_Array : API_Property_Array;
                                 AI_Properties  : out AI_Material_Property_List)
                                 return Assimp_Types.API_Return is
      use Interfaces.C;
      use Assimp_Types;
      aProperty      : API_Material_Property;
      AI_Property    : AI_Material_Property;
      Result         : API_Return := API_Return_Failure;
   begin
      for Property_Index in unsigned range 1 .. Property_Array'Length loop
         aProperty := Property_Array (Property_Index);
         Result := To_AI_Property (anAPI_Material, aProperty, AI_Property);
         if Result = API_Return_Success then
            Assimp_Util.Print_AI_Property_Data ("Material.To_AI_Property_List",
                                                AI_Property);
            AI_Properties.Append (AI_Property);
         else
            raise Material_Exception with
              "Material.To_AI_Property_List failed for property: "
               & unsigned'Image (Property_Index);
         end if;
      end loop;

      return Result;

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
      Property_Array           : API_Property_Array
        (1 .. unsigned (Length (aMaterial.Properties)));
      Index               : unsigned := 0;
      aProperty           : AI_Material_Property;
      API_Property_Record : API_Material_Property;
   begin
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

end Material;
