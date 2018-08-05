
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;
with API_Vectors_Matrices;

package Camera is

   type AI_Camera_Map is private;

   type API_Camera is record
      Name           : Assimp_Types.AI_String;
      Position       : API_Vectors_Matrices.API_Vector_3D;
      Up             : API_Vectors_Matrices.API_Vector_3D;
      Look_At        : API_Vectors_Matrices.API_Vector_3D;
      Horizontal_FOV : Interfaces.C.C_float := 0.0;
      Clip_Pane_Near : Interfaces.C.C_float := 0.0;
      Clip_Pane_Far  : Interfaces.C.C_float := 0.0;
      Aspect         : Interfaces.C.C_float := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Camera);

   type API_Camera_Array is array (Interfaces.C.unsigned range <>) of
     aliased API_Camera;
   pragma Convention (C, API_Camera_Array);

   package Camera_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Camera, API_Camera_Array, API_Camera'(others => <>));

   function To_AI_Camera_Map (Num_Cameras : Interfaces.C.unsigned := 0;
                              C_Array : API_Camera_Array)
                              return AI_Camera_Map;
private

   type AI_Camera is record
      Name           : Ada.Strings.Unbounded.Unbounded_String;
      Position       : GL.Types.Singles.Vector3;
      Up             : GL.Types.Singles.Vector3;
      Look_At        : GL.Types.Singles.Vector3;
      Horizontal_FOV : GL.Types.Single := 0.0;
      Clip_Pane_Near : GL.Types.Single := 0.0;
      Clip_Pane_Far  : GL.Types.Single := 0.0;
      Aspect         : GL.Types.Single := 0.0;
   end record;

   package AI_Camera_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Camera);
   type AI_Camera_Map is new AI_Camera_Package.Map with null Record;

end Camera;
