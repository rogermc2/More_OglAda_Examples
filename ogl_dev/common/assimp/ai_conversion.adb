
with Interfaces.C.Strings;

with Ada.Containers;

with GL.Types;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;
with Assimp_Types;
with Material_Keys;
with Utilities;

with Material_Keys;

package body AI_Conversion is

    procedure To_AI_Property_List (anAPI_Material : Material.API_Material;
                                   Property_Ptr_Array : Material.API_Property_Ptr_Array;
                                   AI_Properties  : out Material.AI_Material_Property_List);

    --     procedure To_API_Property (aProperty       : Material.AI_Material_Property;
    --                                Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
    --                                theAPI_Property : in out Material.API_Material_Property);
    --     procedure To_API_Material (aMaterial       : AI_Material;
    --                                theAPI_Material : in out API_Material);

    --     procedure Load_API_Property_Array (Properties     : Material.AI_Material_Property_List;
    --                                        Property_Array : in out Material.Material_Property_Array);

    --  -------------------------------------------------------------------------

    --     procedure Load_API_Property_Array (Properties     : Material.AI_Material_Property_List;
    --                                        Property_Array : in out Material.Material_Property_Array) is
    --        use Interfaces.C;
    --        use  Material.AI_Material_Property_Package;
    --        use Assimp_Types.Byte_Data_Package;
    --        Property_Cursor      :  Material.AI_Material_Property_Package.Cursor :=
    --                                 Properties.First;
    --        Data_Cursor          : Assimp_Types.Byte_Data_Package.Cursor;
    --        Data_Length          : unsigned := 0;
    --        aProperty            :  Material.AI_Material_Property;
    --        Index                : unsigned := 0;
    --     begin
    --        while Has_Element (Property_Cursor) loop
    --           Index := Index + 1;
    --           aProperty := Element (Property_Cursor);
    --           Data_Cursor := aProperty.Data_Buffer.First;
    --           --           Assimp_Util.Print_AI_Property_Data ("AI_Conversion.Load_API_Property_Array", aProperty);
    --           --           New_Line;
    --           Property_Array (index).Key := Assimp_Util.To_Assimp_API_String (aProperty.Key);
    --           Property_Array (index).Semantic := unsigned (aProperty.Semantic);
    --           Property_Array (index).Texture_Index :=
    --             unsigned (aProperty.Texture_Index);
    --           Data_Length := unsigned (aProperty.Data_Buffer.Length);
    --           Property_Array (index).Data_Length := Data_Length;
    --           Property_Array (index).Data_Type := aProperty.Data_Type;
    --           declare
    --              Data       : char_array (1 .. size_t (Data_Length));
    --              Data_Index : size_t := 0;
    --              aChar      : Character;
    --           begin
    --              while Has_Element (Data_Cursor) loop
    --                 Data_Index := Data_Index + 1;
    --                 aChar := To_Ada (char (Element (Data_Cursor)));
    --                 Data (Data_Index) := To_C (aChar);
    --                 Next (Data_Cursor);
    --              end loop;
    --
    --              Property_Array (index).Data_Ptr := Strings.New_Char_Array (Data);
    --           end;
    --           Next (Property_Cursor);
    --        end loop;
    --
    --     exception
    --        when others =>
    --           Put_Line ("An exception occurred in AI_Conversion.Load_API_Property_Array.");
    --           raise;
    --     end Load_API_Property_Array;

    --  ----------------------------------------------------------------------

    function To_AI_Material (C_Material : Material.API_Material) return Material.AI_Material is
        use Interfaces.C;
        use Material;
        Num_Property  : constant unsigned := C_Material.Num_Properties;
        theMaterial   : AI_Material;
    begin
        Put_Line ("Material.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
                    unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
        if Num_Property > 0 then
            declare
                use Property_Ptr_Array_Package;
                theProperties_Ptr_Array : API_Property_Ptr_Array (1 .. Num_Property);
            begin
                theProperties_Ptr_Array := Property_Ptr_Array_Package.Value
                  (C_Material.Properties, ptrdiff_t (Num_Property));
                To_AI_Property_List
                  (C_Material, theProperties_Ptr_Array, theMaterial.Properties);
                theMaterial.Num_Allocated := GL.Types.UInt (C_Material.Num_Allocated);
            end;
        end if;
        return theMaterial;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Material.");
            raise;
    end To_AI_Material;

    --     function To_AI_Material (C_Material : Material.API_Material)
    --                              return Material.AI_Material is
    --        use Interfaces.C;
    --        theMaterial   : Material.AI_Material;
    --     begin
    --        Put_Line ("AI_Conversion.To_AI_Material C_Material.Num_Properties, Num_Allocated: " &
    --                    unsigned'Image (C_Material.Num_Properties) & unsigned'Image (C_Material.Num_Allocated));
    --        To_AI_Property_List
    --          (C_Material, theMaterial.Properties);
    --        theMaterial.Num_Allocated := GL.Types.UInt (C_Material.Num_Allocated);
    --        Assimp_Util.Print_AI_Property_Data ("AI_Conversion.To_AI_Material Property 1",
    --                                            theMaterial.Properties.First_Element);
    --        return theMaterial;
    --
    --     exception
    --        when others =>
    --           Put_Line ("An exception occurred in AI_Conversion.To_AI_Material.");
    --           raise;
    --     end To_AI_Material;

    --  ------------------------------------------------------------------------

    function To_AI_Materials_Map (Num_Materials    : Interfaces.C.unsigned := 0;
                                  C_Material_Array : in out Material.API_Material_Array)
                                 return Material.AI_Material_Map is
        use Interfaces.C;
        Material_Map : Material.AI_Material_Map;
        aMaterial    : Material.AI_Material;
    begin
        Put_Line ("AI_Conversion.To_AI_Materials_Map Num_Materials: " &
                    Interfaces.C.unsigned'Image (Num_Materials));
        for mat in 1 .. Num_Materials loop
            aMaterial := To_AI_Material (C_Material_Array (mat));
            Material_Map.Insert (GL.Types.UInt (mat), aMaterial);
        end loop;
        return Material_Map;

    exception
        when others =>
            Put_Line ("An exception occurred in AI_Conversion.To_AI_Materials_Map.");
            raise;
    end To_AI_Materials_Map;

    --  ------------------------------------------------------------------------

    procedure To_AI_Property (anAPI_Material : Material.API_Material;
                              API_Property   : Material.API_Material_Property;
                              theAI_Property : out Material.AI_Material_Property) is
        use Ada.Containers;
        use Interfaces.C;
        use Interfaces.C.Strings;
        use Ada.Strings.Unbounded;
        use GL.Types;
        use Assimp_Types;
        use Material.API_Property_Array_Package;
        use Raw_Data_Pointers;

        Key_Length        : size_t := API_Property.Key.Length;
        --  Data_Length number of bytes
        Data_Length       : constant size_t := size_t (API_Property.Data_Length);
        Data_Array        : Raw_Byte_Data (1 .. UInt (Data_Length));
        Key_String        : constant String (1 .. Integer (Key_Length)) :=
                              To_Ada (API_Property.Key.Data);
    begin
        if Key_Length > 0 then
            declare
                Key_Data  : constant String (1 .. Integer (Key_Length)) :=
                              To_Ada (API_Property.Key.Data);
            begin
                theAI_Property.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_Data);
            end;
        end if;

        theAI_Property.Semantic := UInt (API_Property.Semantic);
        theAI_Property.Texture_Index := UInt (API_Property.Texture_Index);
        theAI_Property.Data_Type := API_Property.Data_Type;
        theAI_Property.Data_Buffer.Clear;
        Assimp_Util.Print_API_Property_Data ("AI_Conversion.To_AI_Property API", API_Property);
        if Data_Length > 0 then
            if API_Property.Data_Ptr /= null then
                Data_Array := Raw_Data_Pointers.Value (API_Property.Data_Ptr, ptrdiff_t (Data_Length));
                if Data_Array'Length /= Data_Length then
                    Put_Line ("AI_Conversion.To_AI_Property, Data_Array size " & size_t'Image (Data_Array'Length) &
                                " is not equal to Data_Length" & size_t'Image (Data_Length));
                end if;

                for index in 1 .. Data_Length loop
                    theAI_Property.Data_Buffer.Append (Data_Array (UInt (index)));
                end loop;
            else
                raise Conversion_Exception with
                  "AI_Conversion.To_AI_Property AI_Property Data pointer is null.";
            end if;
            if size_t (theAI_Property.Data_Buffer.Length) /= Data_Length then
                Put_Line ("AI_Conversion.To_AI_Property, AI Data_Array size " &
                Ada.Containers.Count_Type'Image (theAI_Property.Data_Buffer.Length) &
                            " is not equal to Data_Length" & size_t'Image (Data_Length));
            end if;

        end if;
        Assimp_Util.Print_AI_Property_Data ("AI_Conversion.To_AI_Property AI",
                                            theAI_Property);

    exception
        when others =>
            Put_Line ("An exception occurred in AI_Conversion.To_AI_Property.");
            raise;

    end To_AI_Property;

    --  ----------------------------------------------------------------------

    --      function To_AI_Property_List (anAPI_Material     : Material.API_Material;
    --                                    Property_Ptr_Array : Material.API_Property_Ptr_Array)
    --                                   return Material.AI_Material_Property_List is

    procedure To_AI_Property_List (anAPI_Material : Material.API_Material;
                                   Property_Ptr_Array : Material.API_Property_Ptr_Array;
                                   AI_Properties  : out Material.AI_Material_Property_List) is
        use Interfaces.C;
        use Material;
        aProperty      : API_Material_Property;
        AI_Property    : AI_Material_Property;
    begin
        Put_Line ("Material.To_AI_Property_List, Property_Ptr_Array'Length" &
                    unsigned'Image (Property_Ptr_Array'Length));
        for Property_Index in unsigned range 1 .. Property_Ptr_Array'Length loop
            aProperty := Property_Ptr_Array (Property_Index).all;
            New_Line;
            Put_Line ("Material.To_AI_Property_List, setting AI Property" &
                        unsigned'Image (Property_Index));
            To_AI_Property (anAPI_Material, aProperty, AI_Property);
            Put_Line ("Material.To_AI_Property_List, appending AI Property" &
                        unsigned'Image (Property_Index));
            AI_Properties.Append (AI_Property);
            Put_Line ("Material.To_AI_Property_List, appended AI Property" &
                        unsigned'Image (Property_Index));
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Material.To_AI_Property_List.");
            raise;

    end To_AI_Property_List;

    --     procedure To_AI_Property_List (anAPI_Material : Material.API_Material;
    --        --                                   Property_Array : Material.API_Property_Array;
    --                                    AI_Properties  : out Material.AI_Material_Property_List) is
    --        use Interfaces.C;
    --        use Assimp_Types;
    --        use Material;
    --        Num_Properties : constant unsigned := anAPI_Material.Num_Properties;
    --        aProperty      : API_Material_Property;
    --        AI_Property    : AI_Material_Property;
    --     begin
    --        if Num_Properties > 0 then
    --           declare
    --              use Material.API_Property_Array_Package;
    --              Property_Array_Ptr  : Material.API_Material_Property_Ptr :=
    --                                      anAPI_Material.Properties.all;
    --              Property_Array      : Material.API_Property_Array (1 .. Num_Properties);
    --           begin
    --              Property_Array := Material.API_Property_Array_Package.Value
    --                (Property_Array_Ptr, ptrdiff_t (Num_Properties));
    --              New_Line;
    --              Put_Line ("AI_Conversion.To_AI_Property_List, Property_Array'Length, Num_Properties:"
    --                        & unsigned'Image (Property_Array'Length) & "  "
    --                        & unsigned'Image (Num_Properties));
    --              for Property_Index in unsigned range 1 .. Num_Properties loop
    --                 Put_Line ("AI_Conversion.To_AI_Property_List, Property_Index:"
    --                           & unsigned'Image (Property_Index));
    --                 aProperty := Property_Array (Property_Index);
    --                 Assimp_Util.Print_API_Property_Data ("AI_Conversion.To_AI_Property_List aProperty",
    --                                                      aProperty);
    --                 To_AI_Property (anAPI_Material, aProperty, AI_Property);
    --                 --              Assimp_Util.Print_AI_Property_Data ("AI_Conversion.To_AI_Property_List",
    --                 --                                                  AI_Property);
    --                 AI_Properties.Append (AI_Property);
    --              end loop;
    --           end;
    --        end if;
    --
    --     exception
    --        when others =>
    --           Put_Line ("An exception occurred in AI_Conversion.To_AI_Property_List.");
    --           raise;
    --
    --     end To_AI_Property_List;

    --  ----------------------------------------------------------------------

    --     procedure To_API_Material (aMaterial       : AI_Material;
    --                                theAPI_Material : in out API_Material) is
    --        use Interfaces.C;
    --        use AI_Material_Property_Package;
    --        Property_Curs       : Cursor := aMaterial.Properties.First;
    --        Property_Array           : API_Property_Array
    --          (1 .. unsigned (Length (aMaterial.Properties)));
    --        Index               : unsigned := 0;
    --        aProperty           : AI_Material_Property;
    --        API_Property_Record : API_Material_Property;
    --     begin
    --        while Has_Element (Property_Curs) loop
    --           Index := Index + 1;
    --           aProperty := Element (Property_Curs);
    --           declare
    --              --  Raw_Data holds the property's value.
    --              Raw_Data  : Assimp_Types.Raw_Byte_Data
    --                (1 .. UInt (aProperty.Data_Buffer.Length));
    --              --              Raw_Ptr   : Assimp_Types.Data_Pointer := Raw_Data (1)'Access;
    --           begin
    --              To_API_Property (aProperty, Raw_Data, API_Property_Record);
    --              --           theAPI_Material.Properties := API_Property_Record;
    --           end;
    --           Next (Property_Curs);
    --        end loop;
    --
    --        theAPI_Material.Num_Properties := unsigned (Length (aMaterial.Properties));
    --        theAPI_Material.Num_Allocated := unsigned (aMaterial.Num_Allocated);
    --     exception
    --        when others =>
    --           Put_Line ("An exception occurred in AI_Conversion.To_API_Material.");
    --           raise;
    --
    --     end To_API_Material;

    --  ----------------------------------------------------------------------

    --     procedure To_API_Property (aProperty       : Material.AI_Material_Property;
    --                                Raw_Data        : in out Assimp_Types.Raw_Byte_Data;
    --                                theAPI_Property : in out Material.API_Material_Property) is
    --        use Interfaces.C;
    --        use GL.Types;
    --        use Assimp_Types;
    --        use Assimp_Types.Byte_Data_Package;
    --        Name       : String := Ada.Strings.Unbounded.To_String (aProperty.Key);
    --        Data       : Byte_Data_List := aProperty.Data_Buffer;
    --        Curs       : Cursor := Data.First;
    --        Raw_Length : unsigned := unsigned (aProperty.Data_Buffer.Length);
    --        Index      : UInt := 0;
    --     begin
    --        theAPI_Property.Key.Length := Name'Length;
    --        theAPI_Property.Key.Data := To_C (Name);
    --        theAPI_Property.Semantic := unsigned (aProperty.Semantic);
    --        theAPI_Property.Texture_Index := unsigned (aProperty.Texture_Index);
    --        theAPI_Property.Data_Length := Raw_Length;
    --        theAPI_Property.Data_Type := aProperty.Data_Type;
    --        --  Raw_Data holds the property's value. Its size is always Data_Length
    --        while Has_Element (Curs) loop
    --           Index := Index + 1;
    --           Raw_Data (Index) := Element (Curs);
    --           Next (Curs);
    --        end loop;
    --
    --     exception
    --        when others =>
    --           Put_Line ("An exception occurred in AI_Conversion.To_API_Property.");
    --           raise;
    --     end To_API_Property;

    --  ----------------------------------------------------------------------

end AI_Conversion;
