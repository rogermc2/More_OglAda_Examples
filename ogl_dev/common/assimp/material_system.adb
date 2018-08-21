
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;

package body Material_System is

--    function Get_Material_Property (aMaterial : Material.API_Material; Key : Interfaces.C.Strings.chars_ptr;
  function Get_Material_Property (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                 Property_Type, Property_Index : Interfaces.C.unsigned;
                                 theProperty : out Material.API_Material_Property) return Assimp_Types.API_Return is
      use Interfaces.C;
      use Material.Property_Array_Pointers_Package;
      Num_Props  : constant Interfaces.C.unsigned := aMaterial.Num_Properties;
      Result     :  Assimp_Types.API_Return :=  Assimp_Types.API_Return_Failure;
      aProperty  :  Material.API_Material_Property;
      Found      : Boolean := False;
      Prop_Index : unsigned := 0;
   begin
        while Prop_Index < Num_Props and not Found loop
            aProperty := Value (aMaterial.Properties.all) (Prop_Index);
            Found := aProperty.Key.Data = Key.Data and aProperty.Semantic = Property_Type and
              aProperty.Index = Property_Index;
            if Found then
                theProperty := aProperty;
                Result :=  Assimp_Types.API_Return_Success;
            end if;
            Prop_Index := Prop_Index + 1;
        end loop;
        return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Property.");
         raise;
   end Get_Material_Property;

   --  -------------------------------------------------------------------------

   function Get_Material_String (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                 Property_Type, Property_Index : Interfaces.C.unsigned;
                                 Data_String : out Assimp_Types.API_String) return Assimp_Types.API_Return is
      use Interfaces.C;
      use Assimp_Types;
      use Material;
      us_32_Size : constant size_t := Interfaces.unsigned_32'size / 8;  --  us_32 size in bytes
      aProperty  : API_Material_Property;
      Result     : API_Return := API_Return_Failure;
   begin

      Result := Get_Material_Property  (aMaterial, Key, Property_Type, Property_Index, aProperty);
      if Result = Assimp_Types.API_Return_Success then
            if aProperty.Data_Type = Material.PTI_String then
                if aProperty.Data_Length >= 5 then
                    Data_String.Length := Interfaces.C.Strings.strlen (aProperty.Data) / us_32_Size;
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
