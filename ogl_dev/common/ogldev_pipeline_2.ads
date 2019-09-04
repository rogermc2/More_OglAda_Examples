
with GL.Types; use GL.Types;

with Ogldev_Camera;
with Ogldev_Math;

package Ogldev_Pipeline_2 is

   type Pipeline is private;

   function Get_Projection_Transform (P : Pipeline) return Singles.Matrix4;
   function Get_View_Transform (P : in out Pipeline) return Singles.Matrix4;
   function Get_VP_Transform (P : in out Pipeline) return Singles.Matrix4;
   function Get_World_Transform (P : in out Pipeline) return Singles.Matrix4;
   function Get_WP_Transform (P : Pipeline) return Singles.Matrix4;
   function Get_WV_Transform (P : Pipeline) return Singles.Matrix4;
   function Get_WVP_Transform (P : in out Pipeline) return Singles.Matrix4;

   procedure Init_Transforms (P : in out Pipeline);
   procedure Set_Camera (P : in out Pipeline; C : Ogldev_Camera.Camera);
   procedure Set_Camera (P : in out Pipeline;
                         Pos, Target, Up : Singles.Vector3);
   procedure Set_Orthographic_Projection (P : in out Pipeline;
                                          Info : Ogldev_Math.Orthographic_Projection_Info);
   procedure Set_Perspective_Projection (P : in out Pipeline;
                                         Info : Ogldev_Math.Perspective_Projection_Info);
   procedure Set_Rotation (P : in out Pipeline; X, Y, Z : Single);
   procedure Set_Rotation (P : in out Pipeline; S : Singles.Vector3);
   procedure Set_Scale (P : in out Pipeline; S : Single);
   procedure Set_Scale (P : in out Pipeline; X, Y, Z : Single);
   procedure Set_Scale (P : in out Pipeline; S : Singles.Vector3);
   procedure Set_World_Position (P : in out Pipeline; X, Y, Z : Single);
   procedure Set_World_Position (P : in out Pipeline; S : Singles.Vector3);

private

   type Camera_Data is record
      Position : Singles.Vector3 := (0.0, 0.0, 0.0);
      Target   : Singles.Vector3 := (0.0, 0.0, 0.0);
      Up       : Singles.Vector3 := (0.0, 1.0, 0.0);
   end record;

   type Pipeline is record
      Scale                : Singles.Vector3 := (1.0, 1.0, 1.0);
      World_Pos            : Singles.Vector3 := (0.0, 0.0, 0.0);
      Rotation_Info        : Singles.Vector3 := (0.0, 0.0, 0.0);
      Perspective_Info     : Ogldev_Math.Perspective_Projection_Info;
      Orthographic_Info    : Ogldev_Math.Orthographic_Projection_Info;
      Camera               : Camera_Data;
      WVP_Transform        : Singles.Matrix4 := Singles.Identity4;
      VP_Transform         : Singles.Matrix4 := Singles.Identity4;
      WP_Transform         : Singles.Matrix4 := Singles.Identity4;
      WV_Transform         : Singles.Matrix4 := Singles.Identity4;
      World_Transform      : Singles.Matrix4 := Singles.Identity4;
      View_Transform       : Singles.Matrix4 := Singles.Identity4;
      Projection_Transform : Singles.Matrix4 := Singles.Identity4;
   end record;

end Ogldev_Pipeline_2;
