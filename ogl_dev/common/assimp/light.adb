
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Assimp_Util;

package body Light is

    function To_AI_Light_Map (Num_Lights : Interfaces.C.unsigned := 0;
                              C_Array : API_Light_Array) return AI_Light_Map is
        use Interfaces.C;
        aLight     : AI_Light;
        Light_Map  : AI_Light_Map;
    begin
        Put_Line ("Entered To_AI_Light_Map");
        if Num_Lights > 0 then
            Put_Line ("To_AI_Light_Map Num_Lights:" & unsigned'Image (Num_Lights));
            for index in 1 .. Num_Lights loop
                aLight.Name := Ada.Strings.Unbounded.To_Unbounded_String
                  (To_Ada (C_Array (index).Name.Data));
                aLight.Source_Type := C_Array (index).Source_Type;
                aLight.Position :=
                  Assimp_Util.To_OGL_Vector3 (C_Array (index).Position);
                aLight.Direction :=
                  Assimp_Util.To_OGL_Vector3 (C_Array (index).Direction);
                aLight.Up :=
                  Assimp_Util.To_OGL_Vector3 (C_Array (index).Up);
                aLight.Attenuation_Constant :=
                  Single (C_Array (index).Attenuation_Constant);
                aLight.Attenuation_Linear :=
                  Single (C_Array (index).Attenuation_Linear);
                aLight.Attenuation_Quadratic :=
                  Single (C_Array (index).Attenuation_Quadratic);
                aLight.Colour_Diffuse :=
                  Assimp_Util.To_Colour3D (C_Array (index).Colour_Diffuse);
                aLight.Colour_Ambient :=
                  Assimp_Util.To_Colour3D (C_Array (index).Colour_Ambient);
                aLight.Angle_Inner_Cone :=
                  Single (C_Array (index).Angle_Inner_Cone);
                aLight.Angle_Outer_Cone :=
                  Single (C_Array (index).Angle_Outer_Cone);
                aLight.Size :=
                  Assimp_Util.To_OGL_Vector2 (C_Array (index).Size);
                Light_Map.Insert (UInt (index), aLight);
            end loop;
        end if;
        return Light_Map;

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Import_File.");
            raise;
    end To_AI_Light_Map;

end Light;
