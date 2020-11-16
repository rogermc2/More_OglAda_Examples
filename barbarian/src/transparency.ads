
with GL.Types;

package Transparency is

   type Tr_Item_Type is (Tr_Undef, Tr_Sprite, Tr_Prop);

   procedure Reset_TR_List (Camera_Position : GL.Types.Singles.Vector3);

end Transparency;
