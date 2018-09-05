
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Material_System is

  function Get_Material_Property (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                 Property_Type, Property_Index : Interfaces.C.unsigned;
                                 theProperty : out Material.API_Material_Property)
                                 return Assimp_Types.API_Return is
      use Interfaces.C;
      use Material;
      use Material.Property_Array_Pointers_Package;
      Property_Array_Ptr  :  constant Material.API_Property_Array_Ptr := aMaterial.Properties.all;
      Num_Props  : constant Interfaces.C.unsigned := aMaterial.Num_Properties;
      Result     :  Assimp_Types.API_Return :=  Assimp_Types.API_Return_Failure;
      aProperty  :  Material.API_Material_Property;
      Found      : Boolean := False;
      Prop_Index : unsigned := 0;
   begin
--        Put_Line ("Material_System.Get_Material_Property, Num_Props: " & unsigned'Image (Num_Props));
--        Put_Line ("Material_System.Get_Material_Property, requested Key.Length, Property_Type, Property_Index: " &
--                    size_t'Image (Key.Length) & unsigned'Image (Property_Type) &
--                    unsigned'Image (Property_Index));
      if Property_Array_Ptr = null then
            raise Interfaces.C.Strings.Dereference_Error with
              "Material_System.Get_Material_Property, Property_Array_Ptr is null";

      elsif Num_Props > 0 then
            declare
                use Interfaces.C.Strings;
                Property_Array  : Material.API_Property_Array (1 .. Num_Props);
            begin
                Property_Array := Value (Property_Array_Ptr, ptrdiff_t (Num_Props));
                while Prop_Index < Num_Props and not Found loop
                    Prop_Index := Prop_Index + 1;
--                      Put_Line ("Material_System.Get_Material_Property Prop_Index: " & unsigned'Image (Prop_Index));
                    aProperty := Property_Array (Prop_Index);
--                      Put_Line ("Material_System.Get_Material_Property, Key.Length, Property_Type, Property_Index: " &
--                                  size_t'Image (aProperty.Key.Length) & unsigned'Image (aProperty.Data_Type'Enum_Rep) &
--                                  unsigned'Image (aProperty.Index));
                    Found := aProperty.Key.Data = Key.Data and aProperty.Data_Type'Enum_Rep = Property_Type and
                      aProperty.Index = Property_Index;
                    if Found then
                        theProperty := aProperty;
                        Result :=  Assimp_Types.API_Return_Success;
                    end if;
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

   function Get_Material_String (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                 Property_Type, Property_Index : Interfaces.C.unsigned;
                                 Data_String : out Assimp_Types.API_String)
                                 return Assimp_Types.API_Return is
      use Interfaces.C;
      use Assimp_Types;
      use Material;
      us_32_Size : constant size_t := Interfaces.unsigned_32'size / 8;  --  us_32 size in bytes
--        type Data_Array is new Utilities.Byte_Array;
--        type Property_Object is record
--          Length  : Interfaces.unsigned_32 := 0;
--          Data    : Data_Array;
--        end record;
--        anObject   : Property_Object;
      aProperty     : API_Material_Property;
      Size_String   : String (1 .. 4);
      Result        : API_Return := API_Return_Failure;
   begin
      Data_String.Length := 0;
      Result := Get_Material_Property  (aMaterial, Key, Property_Type, Property_Index, aProperty);
      if Result = Assimp_Types.API_Return_Success then
         Put_Line ("Material_System.Get_Material_String property found Data_Type: " &
                  AI_Property_Type_Info'Image (aProperty.Data_Type));
            if aProperty.Data_Type = Material.PTI_String then
            Put_Line ("Material_System.Get_Material_String PTI_String.");
                if aProperty.Data_Length >= 5 then
                    for index in 1 .. 4 loop
                        Size_String (index) :=  Character'Val (aProperty.Data.all);
                        Raw_Data_Pointers.Increment (aProperty.Data);  --  Data is access Assimp_Types.Raw_Byte_Data;
                    end loop;
                    Put_Line ("Material_System.Get_Material_String : Size_String: " & Size_String);
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
