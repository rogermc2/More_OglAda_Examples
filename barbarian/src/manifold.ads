
with GL.Types;

package Manifold is

   Max_Tile_Cols            : constant Integer := 64;

   Manifold_Exception       : Exception;
   Manifold_Parsing_Exception : Exception;

   function Get_Light_Index (Column, Row  : GL.Types.Int;
                             Light_Number : Integer) return Integer;
   procedure Init;
   procedure Reset_Manifold_Vars;

end Manifold;
