
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;
with API_Vectors_Matrices;

package Light is

   type AI_Light_Map is private;

   type AI_Light_Source_Type is
     (Light_Source_Undefined, Light_Source_Directional, Light_Source_Point,
      Light_Source_Spot, Light_Source_Ambient, Light_Source_Area,
      Light_Source_Force32Bit);
   pragma Convention (C, AI_Light_Source_Type);

   for AI_Light_Source_Type use (Light_Source_Undefined   => 1,
                                 Light_Source_Directional => 2,
                                 Light_Source_Point       => 3,
                                 Light_Source_Spot        => 4,
                                 Light_Source_Ambient     => 5,
                                 Light_Source_Area        => 6,
                                 Light_Source_Force32Bit  => Integer'Last);

   type API_Light is record
      Name                  : Assimp_Types.API_String;
      Source_Type           : AI_Light_Source_Type;
      Position              : API_Vectors_Matrices.API_Vector_3D;
      Direction             : API_Vectors_Matrices.API_Vector_3D;
      Up                    : API_Vectors_Matrices.API_Vector_3D;
      Attenuation_Constant  : Float := 0.0;
      Attenuation_Linear    : Float := 0.0;
      Attenuation_Quadratic : Float := 0.0;
      Colour_Diffuse        : API_Vectors_Matrices.API_Colour_3D;
      Colour_Ambient        : API_Vectors_Matrices.API_Colour_3D;
      Angle_Inner_Cone      : Float := 0.0;
      Angle_Outer_Cone      : Float := 0.0;
      Size                  : API_Vectors_Matrices.API_Vector_2D;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Light);

   type API_Light_Array is array (Interfaces.C.unsigned range <>) of aliased API_Light;
   pragma Convention (C, API_Light_Array);

   package Light_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Light, API_Light_Array, API_Light'(others => <>));

   function To_AI_Light_Map (Num_Lights : Interfaces.C.unsigned := 0;
                             C_Array : API_Light_Array)
                             return AI_Light_Map;

private

   type AI_Light is record
      Name                  : Ada.Strings.Unbounded.Unbounded_String;
      Source_Type           : AI_Light_Source_Type;
      Position              : GL.Types.Singles.Vector3;
      Direction             : GL.Types.Singles.Vector3;
      Up                    : GL.Types.Singles.Vector3;
      Attenuation_Constant  : GL.Types.Single := 0.0;
      Attenuation_Linear    : GL.Types.Single := 0.0;
      Attenuation_Quadratic : GL.Types.Single := 0.0;
      Colour_Diffuse        : GL.Types.Singles.Vector3;
      Colour_Ambient        : GL.Types.Singles.Vector3;
      Angle_Inner_Cone      : GL.Types.Single := 0.0;
      Angle_Outer_Cone      : GL.Types.Single := 0.0;
      Size                  : GL.Types.Singles.Vector2;
   end record;

   package AI_Light_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Light);
   type AI_Light_Map is new AI_Light_Package.Map with null Record;

end Light;
