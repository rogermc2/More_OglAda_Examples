
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;

package body Light is

    function To_AI_Light_Map (Num_Lights : Interfaces.C.unsigned := 0;
                              C_Array_Ptr : Light_Ptr_Array_Pointer) return AI_Light_Map is
        use Interfaces.C;
        use Light_Ptr_Array_Pointers;
        C_Ptrs_Array : API_Light_Ptr_Array :=
                         Value (C_Array_Ptr, ptrdiff_t (Num_Lights));
        aLight     : API_Light;
        anAI_Light : AI_Light;
        Light_Map  : AI_Light_Map;
    begin
        Put_Line ("Entered To_AI_Light_Map");
        if Num_Lights > 0 then
            Put_Line ("To_AI_Light_Map Num_Lights:" & unsigned'Image (Num_Lights));
            for index in 1 .. Num_Lights loop
                aLight := C_Ptrs_Array (index).all;
                anAI_Light.Name := Ada.Strings.Unbounded.To_Unbounded_String
                  (To_Ada (aLight.Name.Data));
                anAI_Light.Source_Type := aLight.Source_Type;
                anAI_Light.Position := Assimp_Util.To_OGL_Vector3 (aLight.Position);
                anAI_Light.Direction := Assimp_Util.To_OGL_Vector3 (aLight.Direction);
                anAI_Light.Up := Assimp_Util.To_OGL_Vector3 (aLight.Up);
                anAI_Light.Attenuation_Constant := Single (aLight.Attenuation_Constant);
                anAI_Light.Attenuation_Linear := Single (aLight.Attenuation_Linear);
                anAI_Light.Attenuation_Quadratic := Single (aLight.Attenuation_Quadratic);
                anAI_Light.Colour_Diffuse := Assimp_Util.To_Colour3D (aLight.Colour_Diffuse);
                anAI_Light.Colour_Ambient := Assimp_Util.To_Colour3D (aLight.Colour_Ambient);
                anAI_Light.Angle_Inner_Cone := Single (aLight.Angle_Inner_Cone);
                anAI_Light.Angle_Outer_Cone := Single (aLight.Angle_Outer_Cone);
                anAI_Light.Size := Assimp_Util.To_OGL_Vector2 (aLight.Size);
                Light_Map.Insert (UInt (index), anAI_Light);
            end loop;
        end if;
        return Light_Map;

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Import_File.");
            raise;
    end To_AI_Light_Map;

end Light;
