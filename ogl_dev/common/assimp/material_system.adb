
with System;

with Interfaces;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;
with Material_Keys;
with Utilities;

with Material_Keys;

package body Material_System is

   function Get_Material_Integer (aMaterial : Material.API_Material;
                                 Key       : Assimp_Types.API_String;
                                 Property_Type  : Material.AI_Property_Type_Info;
                                 Property_Index : Interfaces.C.unsigned;
                                 theInteger : out Interfaces.C.int)
                                  return Assimp_Types.API_Return is
      Result     :  Assimp_Types.API_Return :=  Assimp_Types.API_Return_Failure;
   begin
      theInteger := 0;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Property.");
         raise;
   end Get_Material_Integer;

   --  -------------------------------------------------------------------------

   function Get_Material_Property (aMaterial : Material.API_Material;
                                   Key       : Assimp_Types.API_String;
                                  Property_Type  : Material.AI_Property_Type_Info;
                                  Property_Index : Interfaces.C.unsigned;
                                 theProperty : out Material.API_Material_Property)
                                 return Assimp_Types.API_Return is
      use Interfaces.C;
      use Material;
      use AI_Material_Property_Package;

      Num_Props  : constant UInt := UInt (aMaterial.Properties.Length);
      Properties : AI_Material_Property_List;
      Curs       : Cursor := Properties.First;
      aProperty  : AI_Material_Property;
      Found      : Boolean := False;
      Prop_Index : UInt := 0;
      Result     : Boolean :=  False;
   begin
      if aMaterial.Properties.Is_Empty then
         raise Material_System_Exception with
           "Material_System.Get_Material_Property, aMaterial.Properties is empty";
      else
         while Has_Element (Curs) and not Found loop
            aProperty := Element (Curs);
            Found := aProperty.Key = Key and
              aProperty.Data_Type = Property_Type and
              aProperty.Texture_Index = Property_Index;
            if Found then
               theProperty := aProperty;
               Result :=  True;
            end if;
            Next (Curs);
         end loop;
         if not Found then
            Put ("Material_System.Get_Material_Property; ");
            Put_Line ("Requested property not found.");
         end if;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Property.");
         raise;
   end Get_Material_Property;

   --  -------------------------------------------------------------------------

   function Get_Material_String (aMaterial : Material.API_Material;
                                 Key       : Assimp_Types.API_String;
                                 Property_Type  : Material.AI_Property_Type_Info;
                                 Property_Index : GL.Types.UInt;
                                 Data_String    : out Ada.Strings.Unbounded.Unbounded_String)
                                 return Boolean is
      use Ada.Strings.Unbounded;
      use Material;
      aProperty     : AI_Material_Property;
--        Size_String   : String_4;
      Result        : Boolean := False;
   begin
      Data_String := To_Unbounded_String ("");
      Put_Line ("Material_System.Get_Material_String requested Data_Type: " &
                  AI_Property_Type_Info'Image (Property_Type));
      Result := Get_Material_Property  (aMaterial, Key, Property_Type,
                                        Property_Index, aProperty);
      if Result then
         Put_Line ("Material_System.Get_Material_String property found Data_Type: " &
                     AI_Property_Type_Info'Image (aProperty.Data_Type));
         if aProperty.Data_Type = Material.PTI_String then
            Put_Line ("Material_System.Get_Material_String PTI_String.");
--              if aProperty.Data_Length >= 5 then
--                 for index in 1 .. 4 loop
--                    Size_String (index) := Character (aProperty.Data_Ptr.all);
--                    Raw_Data_Pointers.Increment (aProperty.Data_Ptr);
--                 end loop;
--                 Put_Line ("Material_System.Get_Material_String : Size_String: " &
--                           (String (Size_String)));
--                 Data_String.Length := size_t'Value (String (Size_String));
--                 Put_Line ("Material_System.Get_Material_String : Size: " &
--                             size_t'Image (Data_String.Length));
--                 for index in 5 .. aProperty.Data_Length loop
--                    Data_String.Data (size_t (index - 4)) := char (aProperty.Data_Ptr.all);
--                    Raw_Data_Pointers.Increment (aProperty.Data_Ptr);
--                 end loop;
--              end if;
         end if;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_String.");
         raise;
   end Get_Material_String;

   --  -------------------------------------------------------------------------

   function Get_Texture (aMaterial : Material.API_Material;
                         Tex_Type  : Material.AI_Texture_Type;
                         Tex_Index : GL.Types.UInt := 0;
                         Path      : out Ada.Strings.Unbounded.Unbounded_String)
                         return Assimp_Types.API_Return is
      use Ada.Strings.Unbounded;
      use GL.Types;
      use Assimp_Types;
      use Material_Keys;
      Mapping            : Texture_Mapping := Texture_Mapping_UV;
      UV_Integer         : GL.Types.Int;
      Result             : Assimp_Types.API_Return :=
                             Get_Material_String (aMaterial,
                                                  AI_Material_Key (AI_Mat_Key_Texture_Base),
                             Tex_Type, Tex_Index, Path);
   begin
      if Result = API_Return_Success then
         Result := Get_Material_Integer (aMaterial, AI_Material_Key (AI_Mat_Key_Mapping_Base),
                                         Tex_Type, Tex_Index, UV_Integer);
         Mapping := Texture_Mapping'Enum_Val (UV_Integer);
      else
         Put_Line ("Material.Get_Texture, Get_Material_String failed.");
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Texture.");
         raise;
   end Get_Texture;

   --  -------------------------------------------------------------------------

end Material_System;
