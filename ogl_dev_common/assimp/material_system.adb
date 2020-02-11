
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Material_Keys;

package body Material_System is
    use Material;
    use Assimp_Types;

    function Get_Material_Property (aMaterial      : AI_Material;
                                    Key            : String;
                                    Property_Type  : AI_Texture_Type;
                                    --                                     Property_Type  : AI_Property_Type_Info;
                                    Index          : GL.Types.UInt;
                                    theProperty    : out AI_Material_Property)
                                   return API_Return;

    --  -------------------------------------------------------------------------------------

    function Get_Material_Property (aMaterial      : AI_Material;
                                    Key            : String;
                                    Property_Type  : AI_Texture_Type;
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
        Result     : API_Return :=  Assimp_Types.API_Return_Failure;
    begin
        if aMaterial.Properties.Is_Empty then
            raise Material_System_Exception with
              "Material_System.Get_Material_Property, aMaterial.Properties is empty";
        else
            while Has_Element (Curs) and not Found loop
                aProperty := Element (Curs);
                if Ada.Strings.Unbounded.To_String (aProperty.Key) /= Key then
                    null;
                elsif aProperty.Semantic /= Property_Type then
                    Put_Line ("Material_System.Get_Material_Property, Data_Type test failed." &
                                AI_Texture_Type'Image (aProperty.Semantic));
                elsif aProperty.Texture_Index /= Index then
                    Put_Line ("Material_System.Get_Material_Property, Texture_Index test failed.");
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
                                  Material_Type  : AI_Texture_Type;
                                  theIndex       : GL.Types.UInt;
                                  Data_String    : out
                                    Ada.Strings.Unbounded.Unbounded_String)
                                 return API_Return is
        use Ada.Strings.Unbounded;
        use GL.Types;
        aProperty  : AI_Material_Property;
        Data       : Unbounded_String := To_Unbounded_String ("");
        aChar      : Character;
        index      : UInt := 1;
        Result     : API_Return := API_Return_Failure;
    begin
        Data_String := To_Unbounded_String ("");
        Result := Get_Material_Property (aMaterial, Key, Material_Type,
                                         theIndex, aProperty);
        if Result = API_Return_Success  then
            if aProperty.Data_Type = Material.PTI_String then
                Data := aProperty.Data_Buffer.String_Data;
                aChar := Element (Data, 1);
                while index < UInt (Length (Data)) and
                  aChar /= Character'Val (0) loop
                    Data_String := Data_String & aChar;
                    index := index + 1;
                    aChar := Element (Data, Integer (index));
                end loop;
            end if;
        end if;
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
        use Assimp_Types;
        use Material_Keys;
        Result      : API_Return;
    begin
        --  aiGetMaterialString(mat, AI_MATKEY_TEXTURE(type,index),path) )
        --  #define AI_MATKEY_TEXTURE(type, N) _AI_MATKEY_TEXTURE_BASE,type,N
        --  #define _AI_MATKEY_TEXTURE_BASE  "$tex.file"
        Result := Get_Material_String (aMaterial,
                                       AI_Material_Key (AI_Mat_Key_Texture_Base),
                                       Tex_Type, Tex_Index, Path);
        if Result /= API_Return_Success then
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
