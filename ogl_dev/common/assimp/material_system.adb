
with System;

with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Utilities;

package body Material_System is

   type String_4 is new String (1 .. 4);

   function Get_Material_Property (aMaterial      : Material.API_Material; Key : Assimp_Types.API_String;
                                   Property_Type  : Material.AI_Property_Type_Info;
                                   Property_Index : Interfaces.C.unsigned;
                                   theProperty    : out Material.API_Material_Property)
                                  return Assimp_Types.API_Return is
      use Interfaces.C;
      use Material;
      use Material.Property_Ptr_Array_Package;
      Num_Props  : constant Interfaces.C.unsigned := aMaterial.Num_Properties;
      Result     :  Assimp_Types.API_Return :=  Assimp_Types.API_Return_Failure;
      aProperty  :  Material.API_Material_Property;
      Found      : Boolean := False;
      Prop_Index : unsigned := 0;
   begin
      --         Put_Line (size_t'Image (Key.Length) & " " &
      --                    Material.AI_Property_Type_Info'Image (Property_Type) &
      --                    unsigned'Image (Property_Index));
      if aMaterial.Properties = null then
         raise Interfaces.C.Strings.Dereference_Error with
           "Material_System.Get_Material_Property, aMaterial.Properties is null";

      elsif Num_Props > 0 then
         declare
            use Interfaces.C.Strings;
            Property_Ptr_Array : API_Property_Ptr_Array := Value (aMaterial.Properties);
         begin
            while Prop_Index < Num_Props and not Found loop

               aProperty := Property_Ptr_Array (Prop_Index).all;
               Found := aProperty.Key.Data = Key.Data and aProperty.Data_Type = Property_Type and
                 aProperty.Texture_Index = Property_Index;
               if Found then
                  theProperty := aProperty;
                  Result :=  Assimp_Types.API_Return_Success;
               end if;
               Prop_Index := Prop_Index + 1;
            end loop;
            if not Found then
               Put_Line ("Material_System.Get_Material_Property; Requested property not found.");
            end if;
         end;
      else
         Put_Line ("Material_System.Get_Material_Property; No properties found.");
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Property.");
         raise;
   end Get_Material_Property;

   --  -------------------------------------------------------------------------

   function Get_Material_String (aMaterial      : Material.API_Material;
                                 Key            : Assimp_Types.API_String;
                                 Property_Type  : Material.AI_Property_Type_Info;
                                 Property_Index : Interfaces.C.unsigned;
                                 Data_String    : out Assimp_Types.API_String)
                                 return Assimp_Types.API_Return is
      use Interfaces.C;
      use Assimp_Types;
      use Material;
      aProperty     : API_Material_Property;
      Size_String   : String_4;
      Result        : API_Return := API_Return_Failure;
   begin
      Data_String.Length := 0;
      Put_Line ("Material_System.Get_Material_String requested Data_Type: " &
                  AI_Property_Type_Info'Image (Property_Type));
      Result := Get_Material_Property  (aMaterial, Key, Property_Type,
                                        Property_Index, aProperty);
      if Result = Assimp_Types.API_Return_Success then
         Put_Line ("Material_System.Get_Material_String property found Data_Type: " &
                     AI_Property_Type_Info'Image (aProperty.Data_Type));
         if aProperty.Data_Type = Material.PTI_String then
            Put_Line ("Material_System.Get_Material_String PTI_String.");
            if aProperty.Data_Length >= 5 then
               for index in 1 .. 4 loop
                  Size_String (index) := To_Ada (aProperty.Data_Ptr.all);
                  Raw_Data_Pointers.Increment (aProperty.Data_Ptr);  --  Data is access Assimp_Types.Raw_Byte_Data;
               end loop;
               Put_Line ("Material_System.Get_Material_String : Size_String: " &
                         (String (Size_String)));
            end if;
         end if;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_String.");
         raise;
   end Get_Material_String;

   --  -------------------------------------------------------------------------

end Material_System;
