
with Assimp_Util;

package body Camera is

   function To_AI_Camera_Map (Num_Cameras : Interfaces.C.unsigned := 0;
                              C_Ptr_Array_Ptr : Camera_Ptr_Array_Pointer)
                               return AI_Camera_Map is
      C_Ptr_Array  :constant API_Camera_Ptr_Array :=
                        Value (C_Ptr_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Cameras));
      anAPI_Camera : API_Camera;
      aCamera      : AI_Camera;
      Camera_Map   : AI_Camera_Map;
   begin
      for index in 1 .. Num_Cameras loop
         anAPI_Camera := C_Ptr_Array (index).all;
         aCamera.Name := Ada.Strings.Unbounded.To_Unbounded_String
           (Interfaces.C.To_Ada (anAPI_Camera.Name.Data));
         aCamera.Position :=
           Assimp_Util.To_OGL_Vector3 (anAPI_Camera.Position);
         aCamera.Up :=
           Assimp_Util.To_OGL_Vector3 (anAPI_Camera.Up);
         aCamera.Look_At :=
           Assimp_Util.To_OGL_Vector3 (anAPI_Camera.Look_At);
         aCamera.Horizontal_FOV := Maths.Degree (anAPI_Camera.Horizontal_FOV);
         aCamera.Clip_Pane_Near := Single (anAPI_Camera.Clip_Pane_Near);
         aCamera.Clip_Pane_Far := Single (anAPI_Camera.Clip_Pane_Far);
         aCamera.Aspect := Single (anAPI_Camera.Aspect);
         Camera_Map.Insert (UInt (index), aCamera);
      end loop;
      return Camera_Map;

   end To_AI_Camera_Map;

end Camera;
