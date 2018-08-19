
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;

package body Material is

   function To_AI_Property_List (Num_Properties  : Interfaces.C.unsigned := 0;
                                 Properties_Array_Access : access API_Property_Array_Ptr)
                                 return AI_Material_Property_List;
--      procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material);

   --  -------------------------------------------------------------------------

   procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                          Tex_Index : UInt := 0;
                          Path : out Ada.Strings.Unbounded.Unbounded_String;
                          Result : out Assimp_Types.AI_Return) is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      Material : API_Material;
      C_Path   : aliased Assimp_Types.API_String;
   begin
--        To_API_Material (aMaterial, Material);
      Result :=
        Assimp.API.Get_Material_Texture1
          (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
      Path := To_Unbounded_String (To_Ada (C_Path.Data));

   exception
      when others =>
         Put_Line ("An exception occurred in Material.Get_Texture.");
         raise;
   end Get_Texture;

   --  -------------------------------------------------------------------------

   procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                          Tex_Index : UInt := 0;
                          Path : out Ada.Strings.Unbounded.Unbounded_String;
                          Mapping : AI_Texture_Mapping;
                          UV_Index : out UInt;
                          Blend : out Single;
                          Op : AI_Texture_Op;
                          Map_Mode : AI_Texture_Map_Mode;
                          Result : out Assimp_Types.AI_Return) is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      C_Material : API_Material;
      C_Path     : aliased Assimp_Types.API_String;
      UV         : aliased unsigned;
      C_Blend    : aliased C_float;
   begin
--        To_API_Material (aMaterial, C_Material);
      Result := Assimp.API.Get_Material_Texture
        (C_Material, Tex_Type, unsigned (Tex_Index), C_Path'Access,
          Mapping, UV'Access, C_Blend'Access, Op, Map_Mode);

      UV_Index := UInt (UV);
      Blend := Single (C_Blend);
      Path := To_Unbounded_String (To_Ada (C_Path.Data));

   exception
      when others =>
         Put_Line ("An exception occurred in Material.Get_Texture.");
         raise;
   end Get_Texture;

   --  -------------------------------------------------------------------------

   function Get_Texture_Count (aMaterial : AI_Material;
                               Tex_Type : AI_Texture_Type) return GL.Types.UInt is
--        C_Material : API_Material;
   begin
        return 0;
--        To_API_Material (aMaterial, C_Material);
--        return UInt (Assimp.API.Get_Material_Texture_Count (C_Material, Tex_Type));
   end Get_Texture_Count;

   --  -------------------------------------------------------------------------

   function To_AI_Material (C_Material : API_Material) return AI_Material is
        use Interfaces.C;
        use Property_Array_Pointers_Package;
         theMaterial : AI_Material;
         Property_Array_Access : access API_Property_Array_Ptr := C_Material.Properties;
         Property_Array : API_Property_Array :=
                           Property_Array_Pointers_Package.Value (Property_Array_Access.all, ptrdiff_t (C_Material.Num_Properties));
         Property_Key   : Assimp_Types.API_String := Property_Array (0).Key;
   begin
--        declare
--        begin

         Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                  unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
         Put_Line ("Material.To_AI_Material Property Key Length: " & size_t'Image (Property_Key.Length));
         theMaterial.Properties :=
           To_AI_Property_List (C_Material.Num_Properties, C_Material.Properties);  --  API_Property_Array_Ptr

         Put_Line ("Material.To_AI_Material Properties set.");
         theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
         return theMaterial;
--        end;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Material.");
         raise;
   end To_AI_Material;

   --  ------------------------------------------------------------------------

   function To_AI_Materials_Map (Num_Materials : Interfaces.C.unsigned := 0;
                                 C_Material_Array : API_Material_Array)
                                 return AI_Material_Map is
      use Interfaces.C;
      use Property_Array_Pointers_Package;
      Material_Map      : AI_Material_Map;
      aMaterial         : AI_Material;
      Property_Array_Access : access API_Property_Array_Ptr;
      theProperties_Ptr : API_Property_Array_Ptr;
   begin
      Put_Line ("Material.To_AI_Materials_Map Num_Materials: " &
                  Interfaces.C.unsigned'Image (Num_Materials));
      for mat in 1 .. Num_Materials loop
         Put_Line ("Material.To_AI_Materials_Map Num_Properties: " &
                  Interfaces.C.unsigned'Image (C_Material_Array (mat).Num_Properties));
         Property_Array_Access := C_Material_Array (mat).Properties;
         theProperties_Ptr := Property_Array_Access.all;

         declare
            Property_Array : API_Property_Array :=
                                   Property_Array_Pointers_Package.Value
                                     (theProperties_Ptr, Interfaces.C.ptrdiff_t (C_Material_Array (mat).Num_Properties));
            Property_Key   : Assimp_Types.API_String;
--              Property_Data  : Ada.Strings.Unbounded.Unbounded_String;
             aProperty     : AI_Material_Property;
             theProperties  : AI_Material_Property_List;
         begin
                Put_Line ("Material.To_AI_Materials_Map declare 1 entered");

                for Property_Index in 0 .. C_Material_Array (mat).Num_Properties - 1 loop
                    Property_Key := Property_Array (Property_Index).Key;
                    Put_Line ("Material.To_AI_Materials_Map Property_Key set");
                    declare
                        Key_length   : size_t := Property_Key.Length;
                        Data_length  : unsigned := Property_Array (Property_Index).Data_Length;
                        type API_Key_Data is record
                            Length  : Interfaces.C.size_t := Key_length;
                            Data    : Interfaces.C.char_array (0 .. Key_length) := (others => Interfaces.C.char'Val (0));
                        end record;
                        pragma Convention (C_Pass_By_Copy, API_Key_Data);
                        Key_Data   : API_Key_Data;
--                          Property_Data  : char_array (0 .. size_t (Property_Array (Property_Index).Data_Length)) :=
--                                             (others => Interfaces.C.char'Val (0));
                    begin
                        Put_Line ("Material.To_AI_Materials_Map declare 2 entered");
                        --  Setting Key_String.Data  := Property_Key.Data causes erroneous memory access
                        for index in 0 .. Property_Key.Length loop
                            Key_Data.Data (index) := Property_Key.Data (index);
                        end loop;

                        aProperty.Key := Ada.Strings.Unbounded.To_Unbounded_String (To_Ada (Key_Data.Data));
                        aProperty.Semantic := UInt (Property_Array (Property_Index).Semantic);
                        aProperty.Index := UInt (Property_Array (Property_Index).Index);
                        aProperty.Data_Type := Property_Array (Property_Index).Data_Type;
                        Put_Line ("Material.To_AI_Materials_Map aProperty.Data_Type set");
                        if Data_length > 0 then
                            declare
--                                  C_Data       : Interfaces.C.Strings.chars_ptr;
                                Data_String  : string (1 .. Integer (Data_Length));
                            begin
                                Put_Line ("Material.To_AI_Materials_Map Data_String length: " & Integer'Image (Data_String'Length));
--                                  for index in 0 .. Data_Length loop
--                                      Key_Data.Data (index) := Property_Key (index);
--                                  end loop;
--                                  Data_String := Strings.Value (Property_Array (Property_Index).Data, Data_Length);
--                                  Put_Line ("Material.To_AI_Materials_Map Data_String set");
                                aProperty.Data := Ada.Strings.Unbounded.To_Unbounded_String (Data_String);
                                Put_Line ("Material.To_AI_Materials_Map aProperty.Data set");
                            end;
                        end if;
                        theProperties.Append (aProperty);
                    end;
                     Put_Line ("Material.To_AI_Materials_Map completed Property_Index " & unsigned'Image (Property_Index));
              end loop;
         end;
         Put_Line ("Material.To_AI_Materials_Map Property_Index loop completed");
         New_Line;
         aMaterial := To_AI_Material (C_Material_Array (mat));
         Material_Map.Insert (UInt (mat), aMaterial);
         Put_Line ("Material.To_AI_Materials_Map completed mat " & unsigned'Image (mat));
      end loop;
      return Material_Map;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Materials_Map.");
         raise;
   end To_AI_Materials_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Property (C_Property : API_Material_Property)
                            return AI_Material_Property is
      theProperty : AI_Material_Property;
   begin
      theProperty.Key := Ada.Strings.Unbounded.To_Unbounded_String
        (Interfaces.C.To_Ada (C_Property.Key.Data));
      theProperty.Semantic := UInt (C_Property.Semantic);
      theProperty.Index := UInt (C_Property.Index);
--        theProperty.Data_Length := UInt (C_Property.Data_Length);
--        theProperty.Data_Type := C_Property.Data_Type;
--        theProperty.Data := C_Property.Data;
      return theProperty;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property.");
         raise;
   end To_AI_Property;

   --  ------------------------------------------------------------------------

   function To_AI_Property_List (Num_Properties : Interfaces.C.unsigned := 0;
                                 Properties_Array_Access : access API_Property_Array_Ptr)
                                 return AI_Material_Property_List is
      use Interfaces.C;
      use Property_Array_Pointers_Package;
      Properties_Array_Ptr : API_Property_Array_Ptr:= Properties_Array_Access.all;
      C_Properties         : API_Property_Array (1 .. Num_Properties);
      Properties           : AI_Material_Property_List;
      aProperty            : AI_Material_Property;
   begin
      Put_Line (" Material.To_AI_Property_List, Num_Properties: " &
                  unsigned'Image (Num_Properties));
      C_Properties := Value (Properties_Array_Ptr);
      Put_Line (" Material.To_AI_Property_List C_Properties set.");
      if Num_Properties > 0 then
            for index in 1 .. Num_Properties loop
      Put_Line (" Material.To_AI_Property_List : index: " &
                  unsigned'Image (index));
                aProperty :=
                  To_AI_Property (C_Properties (unsigned (index)));
                Properties.Append (aProperty);
            end loop;
      end if;
      return Properties;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property_List.");
         raise;
   end To_AI_Property_List;

   --  ------------------------------------------------------------------------

--     procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material) is
--        use Interfaces.C;
--        use AI_Material_Property_Package;
--        use Property_Array_Pointers_Package;
--        Properties         : constant  AI_Material_Property_List := aMaterial.Properties;
--        Curs               : Cursor := Properties.First;
--        aProperty          : AI_Material_Property;
--        Property_Array     : API_Property_Array (1 .. unsigned (Properties.Length));
--  --        Property_Array_Ptr : API_Property_Array_Ptr;
--        Prop_Index         : unsigned := 0;
--     begin
--        while Has_Element (Curs) loop
--           Prop_Index := Prop_Index + 1;
--           aProperty := Element (Curs);
--           Property_Array (Prop_Index).Key :=
--             Assimp_Util.To_Assimp_API_String (aProperty.Key);
--           Property_Array (Prop_Index).Semantic := unsigned (aProperty.Semantic);
--           Property_Array (Prop_Index).Index := unsigned (aProperty.Index);
--           Property_Array (Prop_Index).Data_Length := unsigned (aProperty.Data_Length);
--           Property_Array (Prop_Index).Data_Type := aProperty.Data_Type;
--           Property_Array (Prop_Index).Data := aProperty.Data;
--           Next (Curs);
--        end loop;
--  --        Property_Array_Ptr := Property_Array (0)'access;
--  --        theAPI_Material.Properties := Property_Array_Ptr'access;
--        theAPI_Material.Num_Properties := Interfaces.C.unsigned (aMaterial.Num_Allocated);
--        theAPI_Material.Num_Allocated := Interfaces.C.unsigned (aMaterial.Num_Allocated);
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Material.To_API_Material.");
--           raise;
--     end To_API_Material;

   --  ------------------------------------------------------------------------

end Material;
