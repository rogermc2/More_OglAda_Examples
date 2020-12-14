
with GL.Types;

package Transparency is

   type Transparency_Type is (Tr_Undef, Tr_Sprite, Tr_Prop);

   procedure Add_Transparency_Item (Item_Type : Transparency_Type;
                                    Render_ID : GL.Types.Int;
                                    Position  : GL.Types.Singles.Vector3;
                                    Brad      : GL.Types.Single);
   procedure Draw_Transparency_List;
   procedure Reset_Transparency_List (Camera_Position : GL.Types.Singles.Vector3);

end Transparency;
