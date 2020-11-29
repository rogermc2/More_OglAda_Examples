
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types;

with Utilities;

package MGL_Common is

   package UB_Strings_Package is new Ada.Containers.Vectors
     (Positive, Ada.Strings.Unbounded.Unbounded_String);
   type Unbounded_Vector is new UB_Strings_Package.Vector with null record;

   type SDL_Data is record
      Width     : GL.Types.Int;
      Height    : GL.Types.Int;
      Pitch     : GL.Types.Int;
      Data      : Unbounded_Vector;
   end record;

   function Count_Octal_Values (File_Name : String) return Integer;
   procedure Read_SDL_File (File_Name : String; Data : in out SDL_Data);

end MGL_Common;
