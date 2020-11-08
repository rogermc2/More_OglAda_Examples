
with GL.Types;

package Manifold is

   Max_Tile_Cols              : constant Integer := 64;

   Manifold_Exception         : Exception;
   Manifold_Parsing_Exception : Exception;

   procedure Clear_Manifold_Lights;
   procedure Free_Manifold_Mesh_Data;
   function Get_Light_Index (Column, Row  : GL.Types.Int;
                             Light_Number : Integer) return Integer;
   procedure Init;
   procedure Reset_Manifold_Vars;
   procedure Set_Manifold_Ambient_Light (Level : GL.Types.Singles.Vector3);
   procedure Update_Static_Lights_Uniforms;
end Manifold;
