
with System;

with Interfaces;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Utilities;

with Material_Keys;

package body Material_System is

   type String_4 is new String (1 .. 4);

   function Get_Material_Property (aMaterial      : Material.AI_Material;
                                   Key            : Ada.Strings.Unbounded.Unbounded_String;
                                   Property_Type  : Material.AI_Property_Type_Info;
                                   Property_Index : GL.Types.UInt;
                                   theProperty    : out Material.AI_Material_Property)
                                   return Boolean is
      use Ada.Strings.Unbounded;
      use GL.Types;
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
         raise Interfaces.C.Strings.Dereference_Error with
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

   function Get_Material_String (aMaterial      : Material.AI_Material;
                                 Key            : Ada.Strings.Unbounded.Unbounded_String;
                                 Property_Type  : Material.AI_Property_Type_Info;
                                 Property_Index : GL.Types.UInt;
                                 Data_String    : out Ada.Strings.Unbounded.Unbounded_String)
                                 return Boolean is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use Assimp_Types;
      use Material;
      aProperty     : AI_Material_Property;
      Size_String   : String_4;
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
            if aProperty.Data_Length >= 5 then
               for index in 1 .. 4 loop
                  Size_String (index) := Character (aProperty.Data_Ptr.all);
                  Raw_Data_Pointers.Increment (aProperty.Data_Ptr);
               end loop;
               Put_Line ("Material_System.Get_Material_String : Size_String: " &
                         (String (Size_String)));
               Data_String.Length := size_t'Value (String (Size_String));
               Put_Line ("Material_System.Get_Material_String : Size: " &
                           size_t'Image (Data_String.Length));
               for index in 5 .. aProperty.Data_Length loop
                  Data_String.Data (size_t (index - 4)) := char (aProperty.Data_Ptr.all);
                  Raw_Data_Pointers.Increment (aProperty.Data_Ptr);
               end loop;
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

   function Get_Material_Texture (aMaterial : Material.AI_Material;
                                  Tex_Type  : Material.AI_Texture_Type;
                                  Tex_Index : GL.Types.UInt := 0;
                                  Path      : out Ada.Strings.Unbounded.Unbounded_String)
                                  return Boolean is
      use Ada.Strings.Unbounded;
      use GL.Types;
      use Assimp_Types;
      use Material_Keys;
      Properties_Length  : UInt := UInt (aMaterial.Properties.Length);
      Texture_Count      : GL.Types.UInt := 0;
      Result             : Boolean :=
                             Get_Material_String (aMaterial, AI_Material_Key (AI_Mat_Key_Texture_Base), Tex_Type, Tex_Index, Path);
   begin
      if Result then
         Put ("Material.Get_Texture, Assimp_Path characters: ");
         for index in 1 .. Assimp_Path.Length loop
            Assimp_Path.Data (index) := Assimp_Path.Data (index);
            Put (To_Ada (Assimp_Path.Data (index)));
         end loop;
         New_Line;
         Path := Ada.Strings.Unbounded.To_Unbounded_String
           (Assimp_Util.To_String (Assimp_Path));
         Put_Line ("Material.Get_Texture Assimp_Path: " & size_t'Image (Assimp_Path.Length) &
                     "  " &  Assimp_Util.To_String (Assimp_Path));
      else
         Put_Line ("Material.Get_Texture, Get_Material_String failed.");
      end if;
      return Result;
   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Texture_Texture.");
         raise;
   end Get_Material_Texture;

   --  -------------------------------------------------------------------------

end Material_System;
