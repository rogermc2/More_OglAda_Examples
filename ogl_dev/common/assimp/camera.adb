
with Assimp_Util;

package body Camera is

   function To_AI_Camera_Map (Num_Cameras : Interfaces.C.unsigned := 0;
                               C_Array : API_Camera_Array)
                               return AI_Camera_Map is
      aCamera     : AI_Camera;
      Camera_Map  : AI_Camera_Map;
   begin
      for index in 1 .. Num_Cameras loop
         aCamera.Name := Ada.Strings.Unbounded.To_Unbounded_String
           (Interfaces.C.To_Ada (C_Array (index).Name.Data));
         aCamera.Position :=
           Assimp_Util.To_OGL_Vector3 (C_Array (index).Position);
         aCamera.Up :=
           Assimp_Util.To_OGL_Vector3 (C_Array (index).Up);
         aCamera.Look_At :=
           Assimp_Util.To_OGL_Vector3 (C_Array (index).Look_At);
         aCamera.Horizontal_FOV :=
           Single (C_Array (index).Horizontal_FOV);
         aCamera.Clip_Pane_Near :=
           Single (C_Array (index).Clip_Pane_Near);
         aCamera.Clip_Pane_Far :=
           Single (C_Array (index).Clip_Pane_Far);
         aCamera.Aspect :=
           Single (C_Array (index).Aspect);
         Camera_Map.Insert (UInt (index), aCamera);
      end loop;
      return Camera_Map;

   end To_AI_Camera_Map;

end Camera;
