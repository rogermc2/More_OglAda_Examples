
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;

package body Material is

   --      procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material);
   function To_AI_Property_List (theProperties_Ptr : API_Property_Array_Ptr;
                               Num_Property      : Interfaces.C.unsigned)
                               return AI_Material_Property_List;

   --  -------------------------------------------------------------------------

   procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                          Tex_Index : UInt := 0;
                          Path      : out Ada.Strings.Unbounded.Unbounded_String;
                          Result    : out Assimp_Types.AI_Return) is
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
                          Path      : out Ada.Strings.Unbounded.Unbounded_String;
                          Mapping   : AI_Texture_Mapping;
                          UV_Index  : out UInt;
                          Blend     : out Single;
                          Op        : AI_Texture_Op;
                          Map_Mode  : AI_Texture_Map_Mode;
                          Result    : out Assimp_Types.AI_Return) is
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
                               Tex_Type  : AI_Texture_Type) return GL.Types.UInt is
      --        C_Material : API_Material;
   begin
      return 0;
      --        To_API_Material (aMaterial, C_Material);
      --        return UInt (Assimp.API.Get_Material_Texture_Count (C_Material, Tex_Type));
   end Get_Texture_Count;

   --  -------------------------------------------------------------------------

   function To_AI_Property (API_Property : API_Material_Property)
                           return AI_Material_Property is
      use Interfaces.C;
      Key_Data      : String := To_Ada (API_Property.Key.Data);
      aProperty     : AI_Material_Property;
   begin

      aProperty.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
      aProperty.Semantic := UInt (API_Property.Semantic);
      aProperty.Index := UInt (API_Property.Index);
      aProperty.Data_Type := API_Property.Data_Type;
      if API_Property.Data_Length > 0 then
         declare
            Str_Length  : size_t := Strings.Strlen (API_Property.Data);
            Data_String : string (1 .. Integer (Str_Length));
         begin
            Data_String := Strings.Value (API_Property.Data);
            aProperty.Data := Ada.Strings.Unbounded.To_Unbounded_String (Data_String);
         end;
      end if;
      return aProperty;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property.");
         raise;

   end To_AI_Property;

   --  ----------------------------------------------------------------------

   function To_AI_Property_List (theProperties_Ptr : API_Property_Array_Ptr;
                               Num_Property      : Interfaces.C.unsigned)
                               return AI_Material_Property_List is
      use Interfaces.C;
      use Property_Array_Pointers_Package;
      Property_Array : API_Property_Array :=
                       Property_Array_Pointers_Package.Value
                                 (theProperties_Ptr, ptrdiff_t (Num_Property));
      theProperties  : AI_Material_Property_List;
   begin
      for Property_Index in 0 .. Num_Property - 1 loop
          theProperties.Append (To_AI_Property (Property_Array (Property_Index)));
         Put_Line ("Material.Set_Property_List completed Property_Index " &
                     unsigned'Image (Property_Index));
      end loop;
      return theProperties;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property_List.");
         raise;

   end To_AI_Property_List;

   --  ----------------------------------------------------------------------

   function To_AI_Material (C_Material : API_Material) return AI_Material is
      use Interfaces.C;
      use Property_Array_Pointers_Package;
      Property_Array_Access : access API_Property_Array_Ptr;
      theProperties_Ptr     : API_Property_Array_Ptr;
      Num_Property          : unsigned;
      Property_List         : AI_Material_Property_List;
      theMaterial           : AI_Material;
   begin
         Num_Property := C_Material.Num_Properties;
         Put_Line ("Material.To_AI_Material Num_Properties: " &
                     unsigned'Image (Num_Property));
         Property_Array_Access := C_Material.Properties;
         theProperties_Ptr := Property_Array_Access.all;
      Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                  unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
      theMaterial.Properties := To_AI_Property_List ( theProperties_Ptr, Num_Property);

      Put_Line ("Material.To_AI_Material Properties set.");
      theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
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
         Put_Line ("Material.To_AI_Materials_Map completed mat " & unsigned'Image (mat));
      end loop;
      return Material_Map;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Materials_Map.");
         raise;
   end To_AI_Materials_Map;

   --  ------------------------------------------------------------------------

end Material;
