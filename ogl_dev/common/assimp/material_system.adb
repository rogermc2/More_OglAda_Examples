
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
                                   Property_Type  : GL.Types.UInt;
                                   --                                     Property_Type  : AI_Property_Type_Info;
                                   Index          : GL.Types.UInt;
                                   theProperty    : out AI_Material_Property)
                                   return API_Return;

   --  -------------------------------------------------------------------------------------

   function Get_Material_Integer (aMaterial     : AI_Material;
                                  Key           : String;
                                  Property_Type : GL.Types.UInt;
                                  --                                    Property_Type : AI_Property_Type_Info;
                                  Index         : GL.Types.UInt;
                                  theInteger    : out GL.Types.Int)
                                  return API_Return is
      Result      : API_Return :=  Assimp_Types.API_Return_Failure;
      theProperty : AI_Material_Property;
   begin
      theInteger := 0;
      Result := Get_Material_Property (aMaterial, Key, Property_Type,
                                       Index, theProperty);
      if Result = API_Return_Success then
         if theProperty.Data_Type = PTI_Integer then
            theInteger := theProperty.Data_Buffer.Integer_Data;
         elsif theProperty.Data_Type = PTI_Buffer then
            theInteger := theProperty.Data_Buffer.Buffer_Data;
         else
            Put ("Material_System.Get_Material_Integer, ");
            Put_Line ("property type is neither integer nor buffer.");
         end if;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Integer.");
         raise;
   end Get_Material_Integer;

   --  -------------------------------------------------------------------------

   function Get_Material_Property (aMaterial      : AI_Material;
                                   Key            : String;
                                   Property_Type  : GL.Types.UInt;
                                   Index          : GL.Types.UInt;
                                   theProperty    : out AI_Material_Property)
                                   return API_Return is
      use GL.Types;
      use Material;
      use AI_Material_Property_Package;

      Properties : constant AI_Material_Property_List := aMaterial.Properties;
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
         Put_Line ("Material_System.Get_Material_Property requested key: " & Key);
         Put_Line ("Material_System.Get_Material_Property requested type : " &
                     UInt'Image (Property_Type));
         while Has_Element (Curs) and not Found loop
            aProperty := Element (Curs);
            Put_Line ("Material_System.Get_Material_Property aProperty's type: " &
                   UInt'Image (Property_Type));
            Put_Line ("Material_System.Get_Material_Property current key: " &
                   Ada.Strings.Unbounded.To_String (aProperty.Key));
            if Ada.Strings.Unbounded.To_String (aProperty.Key) /= Key then
                Put_Line ("key test failed.");
            elsif aProperty.Semantic /= Property_Type then
               Put_Line ("Data_Type test failed." &
                           UInt'Image (aProperty.Semantic));
            elsif aProperty.Texture_Index /= Index then
               Put_Line ("Texture_Index test failed.");
            else
               Found := True;
            end if;
            if Found then
               theProperty := aProperty;
               Result := Assimp_Types.API_Return_Success;
            end if;
            Next (Curs);
         end loop;
         if not Found then
            Put ("Material_System.Get_Material_Property; ");
            Put_Line ("Requested property not found.");
         end if;
      end if;
      New_Line;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Material_System.Get_Material_Property.");
         raise;
   end Get_Material_Property;

   --  -------------------------------------------------------------------------

   function Get_Material_String (aMaterial      : AI_Material;
                                 Key            : String;
                                 Material_Type  : GL.Types.UInt;
                                 theIndex       : GL.Types.UInt;
                                 Data_String    : out
                                   Ada.Strings.Unbounded.Unbounded_String)
                                 return API_Return is
      aProperty     : AI_Material_Property;
      Result        : API_Return := API_Return_Failure;
   begin
      Data_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Put_Line ("Material_System.Get_Material_String requested Data_Type: " &
                  GL.Types.UInt'Image (Material_Type));
      Result := Get_Material_Property (aMaterial, Key, Material_Type,
                                       theIndex, aProperty);
      if Result = API_Return_Success  then
         Put_Line ("Material_System.Get_Material_String property found Data_Type: " &
                     AI_Property_Type_Info'Image (aProperty.Data_Type));
         if aProperty.Data_Type = Material.PTI_String then
            Put_Line ("Material_System.Get_Material_String PTI_String.");
            Data_String := aProperty.Data_Buffer.String_Data;
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

   --  Based on aiReturn aiGetMaterialTexture
   --   (const C_STRUCT aiMaterial* mat, aiTextureType type, unsigned int index,
   --    C_STRUCT aiString* path)
   function Get_Texture (aMaterial : AI_Material;
                         Tex_Type  : AI_Texture_Type;
                         Tex_Index : GL.Types.UInt := 0;
                         Path      : out Ada.Strings.Unbounded.Unbounded_String)
                         return API_Return is
      use Ada.Strings.Unbounded;
      use GL.Types;
      use Assimp_Types;
      use Material_Keys;
--        Mapping     : Texture_Mapping := Texture_Mapping_UV;
--        UV_Integer  : GL.Types.Int;
      Type_UInt   : constant UInt :=
                      AI_Texture_Type'Enum_Rep (Tex_Type);
      Result      : API_Return;
   begin
      Put_Line ("Material.Get_Texture, Tex_Type: " & UInt'Image (Type_UInt) & "  " &
                  AI_Texture_Type'Image (Tex_Type));
      --  aiGetMaterialString(mat, AI_MATKEY_TEXTURE(type,index),path) )
      --  #define AI_MATKEY_TEXTURE(type, N) _AI_MATKEY_TEXTURE_BASE,type,N
      --  #define _AI_MATKEY_TEXTURE_BASE  "$tex.file"
      Result := Get_Material_String (aMaterial,
                                     AI_Material_Key (AI_Mat_Key_Texture_Base),
                                     Type_UInt, Tex_Index, Path);
      if Result = API_Return_Success then
         --  only required for aiTextureMapping* _mapping input parameter not null
         null;
--           Result := Get_Material_Integer
--             (aMaterial, AI_Material_Key (AI_Mat_Key_Mapping_Base),
--              Type_UInt, Tex_Index, UV_Integer);
--           Mapping := Texture_Mapping'Enum_Val (UV_Integer);
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
