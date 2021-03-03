
with GL.Types;

package Transparency is

   type Transparency_Type is (Transparency_Undef, Transparency_Sprite,
                              Transparency_Prop);

   procedure Add_Transparency_Item (Item_Type : Transparency_Type;
                                    Render_ID : Positive;
                                    Position  : GL.Types.Singles.Vector3;
                                    Brad      : GL.Types.Single);
   procedure Draw_Transparency_List;
   procedure Reset_Transparency_List (Camera_Position : GL.Types.Singles.Vector3);

end Transparency;
