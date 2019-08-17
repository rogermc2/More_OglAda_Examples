
package body Material is

--     type C_Byte  is new Interfaces.C.char;
--     subtype Data_Size is Interfaces.C.unsigned range 0 .. 1024;  --  To avoid possible storage error

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
         if Ada.Strings.Unbounded.To_String (aProperty.Key) = "$tex.file" and
              aProperty.Semantic = Tex_Type then
            Count := Count + 1;
         end if;
         Next (Curs);
      end loop;
      return Count;
   end Get_Texture_Count;

   --  -------------------------------------------------------------------------

end Material;
