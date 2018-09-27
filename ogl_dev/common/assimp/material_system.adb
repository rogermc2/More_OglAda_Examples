
with Interfaces;

with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;
with Material_Keys;
with Utilities;

with Material_Keys;

package body Material_System is
   use Material;
   use Assimp_Types;

   type String_4 is new String (1 .. 4);
   type Byte_Array4 is array (1 .. 4) of GL.Types.UByte;
   type Byte_Array8 is array (1 .. 8) of GL.Types.UByte;

   function Get_Material_Property (aMaterial      : AI_Material;
                                   Key            : String;
                                   Property_Type  : AI_Property_Type_Info;
                                   Index          : GL.Types.UInt;
                                   theProperty    : out AI_Material_Property)
                                   return API_Return;
   function To_Integer (Bytes_In : Byte_Array4) return GL.Types.Int;

   --  -------------------------------------------------------------------------------------

   function Data_To_UB_String (String_Data : Ada.Strings.Unbounded.Unbounded_String) return
     Ada.Strings.Unbounded.Unbounded_String is
      use GL.Types;
      Size_String : String_4;
      Data_Length : UInt;
      theString   : constant String :=
                        Ada.Strings.Unbounded.To_String (String_Data);
      Data_String :  Ada.Strings.Unbounded.Unbounded_String :=
                        Ada.Strings.Unbounded.To_Unbounded_String ("");
   begin
      Data_Length := theString'Length;
      if Data_Length >= 5 then
         Size_String := String_4 (theString (1 .. 4));
         declare
            Str_Data : constant String (1 .. Integer (Data_Length - 4)) :=
                         theString (5 .. Integer (Data_Length));
         begin
            Data_String := Ada.Strings.Unbounded.To_Unbounded_String (Str_Data);
         end;
      end if;
      return Data_String;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Texture.");
         raise;
   end Data_To_UB_String;

   --  -------------------------------------------------------------------------

   function Get_Material_Integer (aMaterial     : AI_Material;
                                  Key           : String;
                                  Property_Type : AI_Property_Type_Info;
                                  Index         : GL.Types.UInt;
                                  theInteger    : out GL.Types.Int)
                                  return API_Return is
      use Assimp_Types.Byte_Data_Package;
      Result      : API_Return :=  Assimp_Types.API_Return_Failure;
      theProperty : AI_Material_Property;
      --        curs        : Cursor := theProperty.Data_Buffer.First;
      --        theData     : Byte_Array4;
      --        aChar       : Character;
   begin
      theInteger := 0;
      Result := Get_Material_Property (aMaterial, Key, Property_Type,
                                       Index, theProperty);
      --        if Result = API_Return_Success then
      --           if theProperty.Data_Type = PTI_Integer or
      --             theProperty.Data_Type = PTI_Buffer then
      --              for index in 1 ..4 loop
      --                 aChar := Character (Element (curs));
      --                 theData (index) := GL.Types.UByte (Character'Pos (aChar));
      --                 Next (curs);
      --              end loop;
      --              theInteger := To_Integer (theData);
      --           else
      --              Put ("Material_System.Get_Material_Integer, ");
      --              Put_Line ("property type is neither integer nor buffer.");
      --           end if;
      --        end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Integer.");
         raise;
   end Get_Material_Integer;

   --  -------------------------------------------------------------------------

   function Get_Material_Property (aMaterial      : AI_Material;
                                   Key            : String;
                                   Property_Type  : AI_Property_Type_Info;
                                   Index          : GL.Types.UInt;
                                   theProperty    : out AI_Material_Property)
                                   return API_Return is
      use GL.Types;
      use Material;
      use AI_Material_Property_Package;

      Num_Props  : constant UInt := UInt (aMaterial.Properties.Length);
      Properties : AI_Material_Property_List;
      Curs       : Cursor := Properties.First;
      aProperty  : AI_Material_Property;
      Found      : Boolean := False;
      Prop_Index : UInt := 0;
      Result     : API_Return :=  Assimp_Types.API_Return_Failure;
   begin
      if aMaterial.Properties.Is_Empty then
         raise Material_System_Exception with
           "Material_System.Get_Material_Property, aMaterial.Properties is empty";
      else
         while Has_Element (Curs) and not Found loop
            aProperty := Element (Curs);
            Put ("Material_System.Get_Material_Property aProperty's type: " &
                   AI_Property_Type_Info'Image (Property_Type));
            Found := Ada.Strings.Unbounded.To_String (aProperty.Key) = Key and
              aProperty.Data_Type = Property_Type and
              aProperty.Texture_Index = Index;
            if Found then
               theProperty := aProperty;
               Result :=  Assimp_Types.API_Return_Success;
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

   function Get_Material_String (aMaterial      : AI_Material;
                                 Key            : String;
                                 Property_Type  : AI_Property_Type_Info;
                                 Property_Index : GL.Types.UInt;
                                 Data_String    : out
                                   Ada.Strings.Unbounded.Unbounded_String)
                                 return API_Return is
      use Ada.Containers;
      use GL.Types;
      use Material;
      use Byte_Data_Package;
      aProperty     : AI_Material_Property;
      --          Prop_Type     : AI_Property_Type_Info := AI_Property_Type_Info'Enum_Val (Property_Type);
      Buffer        : Data := aProperty.Data_Buffer;
      String_Data   : Ada.Strings.Unbounded.Unbounded_String;
      Result        : API_Return := API_Return_Failure;
   begin
      Data_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Put_Line ("Material_System.Get_Material_String requested Data_Type: " &
                  AI_Property_Type_Info'Image (Property_Type));
      Result := Get_Material_Property  (aMaterial, Key, Property_Type,
                                        Property_Index, aProperty);
      if Result = API_Return_Success  then
         Put_Line ("Material_System.Get_Material_String property found Data_Type: " &
                     AI_Property_Type_Info'Image (aProperty.Data_Type));
         --           Data_Length := GL.Types.UInt (Buffer.Length - 4);
         if aProperty.Data_Type = Material.PTI_String then
            Put_Line ("Material_System.Get_Material_String PTI_String.");
            Data_String := Data_To_UB_String
              (aProperty.Data_Buffer.String_Data);
         end if;
      end if;
      Put_Line ("Material_System.Get_Material_String Data_String: " &
                  Ada.Strings.Unbounded.To_String (Data_String));
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_String.");
         raise;
   end Get_Material_String;

   --  -------------------------------------------------------------------------

   function Get_Texture (aMaterial : AI_Material;
                         Tex_Type  : AI_Texture_Type;
                         Tex_Index : GL.Types.UInt := 0;
                         Path      : out Ada.Strings.Unbounded.Unbounded_String)
                            return API_Return is
      use Ada.Strings.Unbounded;
      use GL.Types;
      use Assimp_Types;
      use Material_Keys;
      Property_Type      : AI_Property_Type_Info;
      Mapping            : Texture_Mapping := Texture_Mapping_UV;
      UV_Integer         : GL.Types.Int;
      Result             : API_Return;
   begin
      Property_Type  :=
        AI_Property_Type_Info'Enum_Val (AI_Texture_Type'Enum_Rep (Tex_Type));
      Result := Get_Material_String (aMaterial,
                                     AI_Material_Key (AI_Mat_Key_Texture_Base),
                                     Property_Type, Tex_Index, Path);
      if Result = API_Return_Success then
         Result := Get_Material_Integer (aMaterial, AI_Material_Key (AI_Mat_Key_Mapping_Base),
                                         Property_Type
                                         , Tex_Index, UV_Integer);
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

   function To_Integer (Bytes_In : Byte_Array4) return GL.Types.Int is
      use Interfaces;
      Int32  : Unsigned_32 := 0;
      Int8   : Unsigned_8;
   begin
      for index in 0 .. 3  loop
         Int8 := Unsigned_8 (Bytes_In (4 - index));
         Int8 := Rotate_Left (Int8, 4);
         Int32 := Int32 + Unsigned_32 (Int8);
         if index /= 3 then
            Int32 := Shift_Left (Int32, 8);
         end if;
      end loop;
      return GL.Types.Int (Int32);

   end To_Integer;

   --  -------------------------------------------------------------------------

end Material_System;
