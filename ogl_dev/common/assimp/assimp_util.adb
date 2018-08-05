
with Interfaces.C;

package body Assimp_Util is

   function To_Assimp_AI_String
     (UB_String :  Ada.Strings.Unbounded.Unbounded_String)
      return Assimp_Types.AI_String is
      use Interfaces.C;
      theString     : constant String := Ada.Strings.Unbounded.To_String (UB_String);
      Assimp_String : Assimp_Types.AI_String;
   begin
      Assimp_String.Length := theString'Length;
      for index in 1 ..  Assimp_String.Length loop
         Assimp_String.Data (index - 1) := To_C (theString (Integer (index)));
      end loop;
      Assimp_String.Data (Assimp_String.Length) := nul;
      return Assimp_String;
   end To_Assimp_AI_String;

   --  ------------------------------------------------------------------------

   function To_OGL_Vector2 (C_Vec : API_Vectors.API_Vector_2D)
                            return Singles.Vector2 is
      Vec : Singles.Vector2;
   begin
      Vec (GL.X) := Single (C_Vec.X);
      Vec (GL.Y) := Single (C_Vec.Y);
      return Vec;
   end To_OGL_Vector2;

   --  ------------------------------------------------------------------------

   function To_OGL_Vector3 (C_Vec : API_Vectors.API_Vector_3D)
                            return Singles.Vector3 is
      Vec : Singles.Vector3;
   begin
      Vec (GL.X) := Single (C_Vec.X);
      Vec (GL.Y) := Single (C_Vec.Y);
      Vec (GL.Z) := Single (C_Vec.Z);
      return Vec;
   end To_OGL_Vector3;

   --  ------------------------------------------------------------------------

   function To_Colour3D (C_Colours : API_Vectors.API_Colour_3D)
                          return Singles.Vector3 is
      theColours : Singles.Vector3;
   begin
      theColours :=
        (Single (C_Colours.R), Single (C_Colours.G), Single (C_Colours.B));
      return theColours;
   end To_Colour3D;

   --  ------------------------------------------------------------------------

   function To_Colour4D (C_Colours : API_Vectors.API_Colour_4D)
                          return Singles.Vector4 is
      theColours : Singles.Vector4;
   begin
      theColours :=
        (Single (C_Colours.R), Single (C_Colours.G),
         Single (C_Colours.B), Single (C_Colours.A));
      return theColours;
   end To_Colour4D;

   --  ------------------------------------------------------------------------

end Assimp_Util;
