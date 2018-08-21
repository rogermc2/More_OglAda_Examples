
package body Material_System is

--    function Get_Material_Property (aMaterial : Material.API_Material; Key : Interfaces.C.Strings.chars_ptr;
  function Get_Material_Property (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                 Property_Type, Property_Index : Interfaces.C.unsigned;
                                 theProperty : out Material.API_Material_Property) return Assimp_Types.API_Return is
      use Interfaces.C;
      use Material.Property_Array_Pointers_Package;
      Num_Props : constant Interfaces.C.unsigned := aMaterial.Num_Properties;
      Result    :  Assimp_Types.API_Return :=  Assimp_Types.API_Return_Failure;
      aProperty :  Material.API_Material_Property;
      Found     : Boolean := False;
   begin
        for index in 1 .. Num_Props loop
            aProperty := Value (aMaterial.Properties.all) (index);
            Found := aProperty.Key.Data = Key.Data and aProperty.Semantic = Property_Type and
              aProperty.Index = Property_Index;
            if Found then
                theProperty := aProperty;
                Result :=  Assimp_Types.API_Return_Success;
            end if;
        end loop;
        return Result;
   end Get_Material_Property;

   --  -------------------------------------------------------------------------

   function Get_Material_String (aMaterial : Material.API_Material; Key : out Interfaces.C.Strings.chars_ptr;
                                 Property_Type, Property_Index : out Interfaces.C.unsigned;
                                 Data_String : out Assimp_Types.API_String) return Assimp_Types.API_Return is
      aProperty :  Material.API_Material_Property;
   begin
      return Assimp_Types.API_Return_Failure;
   end Get_Material_String;

   --  -------------------------------------------------------------------------

end Material_System;
