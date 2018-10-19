
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

end Material;
