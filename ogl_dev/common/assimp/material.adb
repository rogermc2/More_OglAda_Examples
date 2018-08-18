
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;

package body Material is

   function To_AI_Property_List (Num_Properties : Interfaces.C.unsigned := 0;
                                 C_Properties_Array : API_Property_Array_Ptr)
                                 return AI_Material_Property_List;
    procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material);

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
      To_API_Material (aMaterial, Material);
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
      To_API_Material (aMaterial, C_Material);
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
      C_Material : API_Material;
   begin
      To_API_Material (aMaterial, C_Material);
      return UInt (Assimp.API.Get_Material_Texture_Count (C_Material, Tex_Type));
   end Get_Texture_Count;

   --  -------------------------------------------------------------------------

   function To_AI_Material (C_Material : in out API_Material) return AI_Material is
   begin
      declare
         theMaterial : AI_Material;
      begin
         theMaterial.Properties :=
           To_AI_Property_List (C_Material.Num_Properties,
                                C_Material.Properties.all);  --  API_Property_Array_Ptr
         theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
         return theMaterial;
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_API_Material.");
         raise;
   end To_AI_Material;

   --  ------------------------------------------------------------------------

   function To_AI_Property (C_Property : API_Material_Property)
                            return AI_Material_Property is
      theProperty : AI_Material_Property;
   begin
      theProperty.Key := Ada.Strings.Unbounded.To_Unbounded_String
        (Interfaces.C.To_Ada (C_Property.Key.Data));
      theProperty.Semantic := UInt (C_Property.Semantic);
      theProperty.Index := UInt (C_Property.Index);
      theProperty.Data_Length := UInt (C_Property.Data_Length);
      theProperty.Data_Type := C_Property.Data_Type;
      theProperty.Data := C_Property.Data;
      return theProperty;

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Property.");
         raise;
   end To_AI_Property;

   --  ------------------------------------------------------------------------

   function To_AI_Property_List (Num_Properties : Interfaces.C.unsigned := 0;
                                 C_Properties_Array : API_Property_Array_Ptr)
                                 return AI_Material_Property_List is
      use Interfaces.C;
      use Property_Array_Pointers_Package;
      C_Properties : API_Property_Array (1 .. Num_Properties);
      Properties : AI_Material_Property_List;
      aProperty  : AI_Material_Property;
   begin
        C_Properties := Value (C_Properties_Array);
      Put_Line (" Material.To_AI_Property_List : Num_Properties: " &
                  unsigned'Image (Num_Properties));
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

   function To_AI_Materials_Map (Num_Materials : Interfaces.C.unsigned := 0;
                                 C_Material_Array : in out API_Material_Array)
                                 return AI_Material_Map is
      Materials  : AI_Material_Map;
      aMaterial  : AI_Material;
   begin
      for index in 1 .. Num_Materials loop
         aMaterial := To_AI_Material (C_Material_Array (index));
         Materials.Insert (UInt (index), aMaterial);
      end loop;
      return Materials;
   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_AI_Materials_Map.");
         raise;
   end To_AI_Materials_Map;

   --  ------------------------------------------------------------------------

   procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material) is
      use Interfaces.C;
      use AI_Material_Property_Package;
      use Property_Array_Pointers_Package;
      Properties         : constant  AI_Material_Property_List := aMaterial.Properties;
      Curs               : Cursor := Properties.First;
      aProperty          : AI_Material_Property;
      Property_Array     : API_Property_Array (1 .. unsigned (Properties.Length));
--        Property_Array_Ptr : API_Property_Array_Ptr;
      Prop_Index         : unsigned := 0;
   begin
      while Has_Element (Curs) loop
         Prop_Index := Prop_Index + 1;
         aProperty := Element (Curs);
         Property_Array (Prop_Index).Key :=
           Assimp_Util.To_Assimp_API_String (aProperty.Key);
         Property_Array (Prop_Index).Semantic := unsigned (aProperty.Semantic);
         Property_Array (Prop_Index).Index := unsigned (aProperty.Index);
         Property_Array (Prop_Index).Data_Length := unsigned (aProperty.Data_Length);
         Property_Array (Prop_Index).Data_Type := aProperty.Data_Type;
         Property_Array (Prop_Index).Data := aProperty.Data;
         Next (Curs);
      end loop;
--        Property_Array_Ptr := Property_Array (0)'access;
--        theAPI_Material.Properties := Property_Array_Ptr'access;
      theAPI_Material.Num_Properties := Interfaces.C.unsigned (aMaterial.Num_Allocated);
      theAPI_Material.Num_Allocated := Interfaces.C.unsigned (aMaterial.Num_Allocated);

   exception
      when others =>
         Put_Line ("An exception occurred in Material.To_API_Material.");
         raise;
   end To_API_Material;

   --  ------------------------------------------------------------------------

end Material;
