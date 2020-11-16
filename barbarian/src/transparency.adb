

package body Transparency is

   Max_Transparent_Items : constant Integer := 256;

   type TR_Node is record
         Closer    : Integer := -1;  --   -1 means we are the first
         Farther   : Integer := -1;  --   -1 means we are the last
         Tr_IType  : Tr_Item_Type := Tr_Undef;
         Render_Id : Integer; --  ID of this item in that renderer
         Sq_Dist   : Float := 0.0;  --  squared dist of the item from the camera
   end record;

   TR_Nodes           : array (1 .. Max_Transparent_Items) of TR_Node;
   TR_Node_Count      : Integer := 0;
   TR_Closest_Node    : Integer := 0;
   TR_Farthest_Node   : Integer := 0;
   TR_Camera_Position : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);

--  ----------------------------------------------------------------------------

   procedure Reset_TR_List (Camera_Position : GL.Types.Singles.Vector3) is
   begin
      TR_Node_Count := 0;
      TR_Closest_Node := 0;
      TR_Farthest_Node := 0;
      TR_Camera_Position := Camera_Position;
   end Reset_TR_List;

--  ----------------------------------------------------------------------------

end Transparency;
