
with GL.Types; use GL.Types;

package Shadows is

    type Shadow_Direction is (Shadow_Dir_Right, Shadow_Dir_Left,
                              Shadow_Dir_Up, Shadow_Dir_Down,
                              Shadow_Dir_Back, Shadow_Dir_Forward);

    procedure Bind_Cube_Shadow_Texture (Slot : Integer);
    function Caster_Position return Singles.Vector3;
    function Init_Shadows return Boolean;

end Shadows;
