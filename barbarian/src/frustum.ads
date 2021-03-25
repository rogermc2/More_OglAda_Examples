
with Maths;

with GL.Types; use  GL.Types;

package Frustum is

    type FB_Effect is (FB_Default,
                       FB_Gold_Flash,
                       FB_Red_Flash,
                       FB_Fadein,
                       FB_Fadeout,
                       FB_Screw,
                       FB_Grey,
                       FB_White_Flash,
                       FB_Green_Flash);

    --  Init debug variables for frustum wireframe
    procedure Draw_Frustum_Box;
    procedure Enable_Frustum_Cull (State : Boolean);
    procedure Enable_Update (State : Boolean);
    function Frustum_Camera_Position return Singles.Vector3;
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
