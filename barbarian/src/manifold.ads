
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;
with GL.Types;

package Manifold is

   Max_Tile_Cols              : constant Integer := 64;

   Manifold_Exception         : Exception;
   Manifold_Parsing_Exception : Exception;

   procedure Clear_Manifold_Lights;
   procedure Draw_Manifold_Around (Camera_Pos : GL.Types.Singles.Vector3;
                                   Radius : GL.Types.Single;
                                   Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                                   Ramp_Spec_Tex : GL.Objects.Textures.Texture);
   procedure Draw_Manifold_Around_Depth_Only;
   procedure Free_Manifold_Meta_Data;
   function Get_Light_Index (Column, Row  : GL.Types.Int;
                             Light_Number : Integer) return GL.Types.Int;
   procedure Init;
   function Is_Ramp (Row, Col : GL.Types.Int) return Boolean;
   function Is_Water (Row, Col : GL.Types.Int) return Boolean;
   procedure Reset_Manifold_Vars;
   procedure Set_Manifold_Ambient_Light (Level : GL.Types.Singles.Vector3);
   procedure Update_Static_Lights_Uniforms;
end Manifold;
