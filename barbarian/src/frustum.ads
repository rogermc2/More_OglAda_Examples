
with Maths;

with GL.Types; use  GL.Types;

package Frustum is

    --  Init debug variables for frustum wireframe
    procedure Draw_Frustum_Box;
    procedure Enable_Frustum_Cull (State : Boolean);
    procedure Enable_Update (State : Boolean);
    --  Set to false to stop frustum culling alogether for testing
    function Frustum_Cull_Enabled return Boolean;
    --  Set To False To Stop Frustum Plane Extraction
    function Frustum_Update_Enabled return Boolean;
    procedure Init;
    function Is_Sphere_In_Frustum (Centre : Singles.Vector3;  Radius : Single)
                                   return Boolean;
    function Is_Aabb_In_Frustum (Mins, Maxs : in out Singles.Vector3)
                                 return Boolean;
    procedure Re_Extract_Frustum_Planes
      (Fovy_Deg : Maths.Degree; Aspect, Near, Far : Single;
       Cam_Pos  : Singles.Vector3; Mat : Singles.Matrix4);

end Frustum;
