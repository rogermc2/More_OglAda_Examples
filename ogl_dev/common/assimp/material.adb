
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Util;
with Material_System;

package body Material is

    --      procedure To_API_Material (aMaterial : AI_Material; theAPI_Material : in out API_Material);
    function To_AI_Property_List (anAPI_Material : API_Material;
                                  theProperties_Ptr : API_Property_Array_Ptr;
                                  Num_Property      : Interfaces.C.unsigned)
                                 return AI_Material_Property_List;

    --  -------------------------------------------------------------------------

    procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                           Tex_Index : UInt := 0;
                           Path      : out Ada.Strings.Unbounded.Unbounded_String;
                           Result    : out Assimp_Types.ApI_Return) is
        use Interfaces.C;
        use Ada.Strings.Unbounded;
        Material : API_Material;
        C_Path   : aliased Assimp_Types.API_String;
    begin
        --        To_API_Material (aMaterial, Material);
        Result :=
          Assimp.API.Get_Material_Texture1
            (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
        Path := To_Unbounded_String (To_Ada (C_Path.Data));

    exception
        when others =>
            Put_Line ("An exception occurred in Material.Get_Texture.");
            raise;
    end Get_Texture;

    --  -------------------------------------------------------------------------

    procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
                           Tex_Index : UInt := 0;
                           Path      : out Ada.Strings.Unbounded.Unbounded_String;
                           Mapping   : AI_Texture_Mapping;
                           UV_Index  : out UInt;
                           Blend     : out Single;
                           Op        : AI_Texture_Op;
                           Map_Mode  : AI_Texture_Map_Mode;
                           Result    : out Assimp_Types.API_Return) is
        use Interfaces.C;
        use Ada.Strings.Unbounded;
        C_Material : API_Material;
        C_Path     : aliased Assimp_Types.API_String;
        UV         : aliased unsigned;
        C_Blend    : aliased C_float;
    begin
        Result := Assimp.API.Get_Material_Texture
          (C_Material, Tex_Type, unsigned (Tex_Index), C_Path'Access,
           Mapping, UV'Access, C_Blend'Access, Op, Map_Mode);

        UV_Index := UInt (UV);
        Blend := Single (C_Blend);
        Path := To_Unbounded_String (To_Ada (C_Path.Data));

    exception
        when others =>
            Put_Line ("An exception occurred in Material.Get_Texture.");
            raise;
    end Get_Texture;

    --  -------------------------------------------------------------------------

    function Get_Texture_Count (aMaterial : AI_Material;
                                Tex_Type  : AI_Texture_Type) return GL.Types.UInt is
    begin
        return 0;
    end Get_Texture_Count;

    --  -------------------------------------------------------------------------

    function To_AI_Material (C_Material : API_Material) return AI_Material is
        use Interfaces.C;
        use Property_Array_Pointers_Package;
        theProperties_Ptr     : API_Property_Array_Ptr;
        Num_Property          : unsigned;
        Property_List         : AI_Material_Property_List;
        theMaterial           : AI_Material;
    begin
        Num_Property := C_Material.Num_Properties;
        Put_Line ("Material.To_AI_Material Num_Properties: " &
                    unsigned'Image (Num_Property));
        theProperties_Ptr := C_Material.Properties.all;
        Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                    unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
        theMaterial.Properties := To_AI_Property_List (C_Material, theProperties_Ptr, Num_Property);

        theMaterial.Num_Allocated := UInt (C_Material.Num_Allocated);
        return theMaterial;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Material.");
            raise;
    end To_AI_Material;

    --  ------------------------------------------------------------------------

    function To_AI_Materials_Map (Num_Materials    : Interfaces.C.unsigned := 0;
                                  C_Material_Array : API_Material_Array)
                                 return AI_Material_Map is
        use Interfaces.C;
        Material_Map : AI_Material_Map;
        aMaterial    : AI_Material;
    begin
        Put_Line ("Material.To_AI_Materials_Map Num_Materials: " &
                    Interfaces.C.unsigned'Image (Num_Materials));
        for mat in 0 .. Num_Materials - 1 loop
            aMaterial := To_AI_Material (C_Material_Array (mat));
            Material_Map.Insert (UInt (mat), aMaterial);
        end loop;
        return Material_Map;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Materials_Map.");
            raise;
    end To_AI_Materials_Map;

    --  ------------------------------------------------------------------------

    function To_AI_Property (anAPI_Material : API_Material;
                             API_Property : API_Material_Property) return AI_Material_Property is
        use Interfaces.C;
        use Interfaces.C.Strings;
        use Assimp_Types;
        API_Prop     : API_Material_Property := API_Property;
        Key_Length   : constant size_t := API_Prop.Key.Length;
        AI_Property  : AI_Material_Property;
        Data_Length  : constant size_t := size_t (API_Prop.Data_Length);
        Data_String  : Assimp_Types.API_String;
        Raw_Data     : AI_Material_Property_List;
        Result       : Assimp_Types.API_Return := Assimp_Types.API_Return_Failure;
    begin
        Put_Line ("Material.To_AI_Property Key_Length: " & size_t'Image (Key_Length));
        Put_Line ("Material.To_AI_Property Data_Type: " &
                  AI_Property_Type_Info'Image (API_Property.Data_Type));
        if Key_Length > 0 then
            declare
                Key_Data  : constant String (1 .. Integer (Key_Length)) := To_Ada (API_Prop.Key.Data);
            begin
                Put_Line ("Material.To_AI_Property Key_Data: " & Key_Data);
                AI_Property.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
            end;
        end if;

        AI_Property.Semantic := UInt (API_Prop.Semantic);
        Put_Line ("Material.To_AI_Property aProperty.Semantic: " & UInt'Image (AI_Property.Semantic));
        AI_Property.Index := UInt (API_Prop.Index);
        AI_Property.Data_Type := API_Prop.Data_Type;
        Put_Line ("Material.To_AI_Property Semantic, Index: " &
                    UInt'Image (AI_Property.Semantic) & UInt'Image (AI_Property.Index));

        if Data_Length > 0  then
           Put_Line ("Material.To_AI_Property Data_Type: " &
                  AI_Property_Type_Info'Image (API_Property.Data_Type));
            Result := Material_System.Get_Material_String (anAPI_Material, API_Property.Key,
                                                           API_Property.Data_Type,
                                                           API_Property.Index, Data_String);
            Put_Line ("Material.To_AI_Property Result: " & API_Return'Image (Result));
            if Result = API_RETURN_SUCCESS then
                Put_Line ("Material.To_AI_Property Data_Length: " & size_t'Image (Data_Length));
                for index in 1 .. Data_Length loop
                    AI_Property.Data_Buffer.Append (API_Prop.Data.all);
                    Raw_Data_Pointers.Increment (API_Prop.Data);  --  Data is access Assimp_Types.Raw_Byte_Data;
                end loop;
            end if;
        end if;
        return AI_Property;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Property.");
            raise;

    end To_AI_Property;

    --  ----------------------------------------------------------------------

    function To_AI_Property_List (anAPI_Material : API_Material;
                                  theProperties_Ptr : API_Property_Array_Ptr;
                                  Num_Property      : Interfaces.C.unsigned)
                                 return AI_Material_Property_List is
        use Interfaces.C;
        use Property_Array_Pointers_Package;
        Property_Array : API_Property_Array :=
                           Property_Array_Pointers_Package.Value
                             (theProperties_Ptr, ptrdiff_t (Num_Property));
        theProperties  : AI_Material_Property_List;
        aProperty      : API_Material_Property;
    begin
        New_line;
        Put_Line ("Material.To_AI_Property_List Num_Property " & unsigned'Image (Num_Property));
        for Property_Index in 0 .. Num_Property - 1 loop
            aProperty := Property_Array (Property_Index);
            Put_Line ("Material.To_AI_Property_List appending Property_Index " &
                        unsigned'Image (Property_Index));
            Put_Line ("Material.To_AI_Property_List Key.Length: " &
                  size_t'Image (Property_Array (Property_Index).Key.Length) &
                  "  " & To_Ada (Property_Array (Property_Index).Key.Data));
            Put_Line ("Material.To_AI_Property_List Data_Length: " &
                  unsigned'Image (Property_Array (Property_Index).Data_Length));
            Put_Line ("Material.To_AI_Property_List Data_Type: " &
                  AI_Property_Type_Info'Image (Property_Array (Property_Index).Data_Type));
            theProperties.Append (To_AI_Property (anAPI_Material, aProperty));
            New_Line;
        end loop;
        return theProperties;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Property_List.");
            raise;

    end To_AI_Property_List;

    --  ----------------------------------------------------------------------

end Material;
