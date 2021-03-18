
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Blending;
with GL.Buffers;
with GL.Toggles;

with Maths;

with Prop_Renderer;
with Sprite_Renderer;

package body Transparency is

   Max_Transparent_Items : constant Integer := 256;

 --  node for sorting transparent items by distance from camera
   type Transparency_Node is record
      Closer    : Natural := 0;  --  0 means that this is the first
      Farther   : Natural := 0;  --  0 means that this is the last
      Tr_Type   : Transparency_Type := Transparency_Undef;
      Render_Id : Positive; --  ID of this item in that renderer
      Sq_Dist   : GL.Types.Single := 0.0;  --  squared dist of the item from the camera
   end record;

   package Transparent_Package is new Ada.Containers.Vectors
     (Positive, Transparency_Node);
   type Transparent_Vector is new Transparent_Package.Vector with null Record;

   TR_Nodes           : Transparent_Vector;
   TR_Closest_Node    : Natural := 0;
   TR_Farthest_Node   : Natural := 0;
   TR_Camera_Position : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);

   --  ----------------------------------------------------------------------------

   procedure Add_Transparency_Item (Item_Type : Transparency_Type;
                                    Render_ID : Positive;
                                    Position  : GL.Types.Singles.Vector3;
                                    Brad      : GL.Types.Single) is
      use GL.Types;
      use Singles;
      use Transparent_Package;
      Dist         : constant Vector3 := TR_Camera_Position - Position;
      Sq_Dist      : constant Single := Maths.Length_Sq (Dist) - Brad ** 2;
      Inspect_Node : Transparency_Node;
      Inspect      : Natural;
      New_Node     : Transparency_Node;
      P_Closer     : Natural;
      Found        : Boolean := False;

      procedure Update_Farthest (Node : in out Transparency_Node) is
      begin
          Node.Farther := TR_Nodes.Last_Index;
      end Update_Farthest;

   begin
      New_Node.Tr_Type := Item_Type;
      New_Node.Render_Id := Integer (Render_ID);
      New_Node.Sq_Dist := Sq_Dist;

      if Is_Empty (TR_Nodes) then
         TR_Nodes.Append (New_Node);
         TR_Closest_Node := 0;
         TR_Farthest_Node := 0;
         Found := True;
      else
         Inspect := TR_Closest_Node;
         while Inspect /= 0 and not Found loop
            Inspect_Node := TR_Nodes.Element (Inspect);
            Found := Sq_Dist < Inspect_Node.Sq_Dist;
            if Found then
               P_Closer :=  Inspect_Node.Closer;
               Inspect_Node.Closer := TR_Nodes.Last_Index;
               New_Node.Closer := P_Closer;
               New_Node.Farther := Inspect;
               if Inspect = TR_Closest_Node then
                  TR_Closest_Node := TR_Nodes.Last_Index;
               else
               TR_Nodes.Update_Element (P_Closer,
                                        Update_Farthest'Access);
               end if;
               TR_Nodes.Replace_Element (Inspect, Inspect_Node);
               TR_Nodes.Append (New_Node);
            end if;
            Inspect := Inspect_Node.Farther;
         end loop;

         if not Found then
            --  The new node is the farthest node
            New_Node.Closer := TR_Farthest_Node;
            New_Node.Farther := 0;
            TR_Nodes.Append (New_Node);
            if TR_Farthest_Node > 0 then
                TR_Nodes.Update_Element (TR_Farthest_Node,
                                         Update_Farthest'Access);
            end if;
            TR_Farthest_Node := TR_Nodes.Last_Index;
         end if;
      end if;

   end Add_Transparency_Item;

   --  ----------------------------------------------------------------------------

   procedure Draw_Transparency_List is
      use GL.Blending;
      use GL.Toggles;
      use GL.Types;
      Curr_Type : constant Transparency_Type := Transparency_Undef;
      Inspect   : Natural := TR_Farthest_Node;
      Item_Type : Transparency_Type;
      Node      : Transparency_Node;
   begin
      Enable (Blend);
      Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
      GL.Buffers.Depth_Mask (False);
      Enable (Depth_Test);

      while Inspect > 0 loop
           Put_Line ("Transparency.Draw_Transparency_List, Inspect: " &
                    Integer'Image (Inspect));
         Node := TR_Nodes.Element (Inspect);
         Item_Type := Node.Tr_Type;
         if Item_Type /= Curr_Type and Item_Type = Transparency_Sprite then
            Sprite_Renderer.Start_Sprite_Rendering;
         end if;
--           if Item_Type = Transparency_Sprite and Node.Render_Id = 6 then
--           if Item_Type = Transparency_Sprite and Node.Render_Id = 11 then
--           if Item_Type = Transparency_Sprite and Node.Render_Id = 12 then
--           if Item_Type = Transparency_Sprite and Node.Render_Id = 18 then
         if Item_Type = Transparency_Sprite then
           Put_Line ("Transparency.Draw_Transparency_List, Render_Sprite: " &
                       Integer'Image (Node.Render_Id));
            Sprite_Renderer.Render_Sprite (Node.Render_Id);
--           else
--             Put_Line ("Transparency.Draw_Transparency_List, Render_Property: " &
--                         Integer'Image (Node.Render_Id));
--              Prop_Renderer.Render_Property (Node.Render_Id);
         end if;
         Inspect := Node.Closer;
      end loop;
      GL.Buffers.Depth_Mask (True);
      Disable (Blend);

   end Draw_Transparency_List;

   --  ----------------------------------------------------------------------------

   procedure Reset_Transparency_List
      (Camera_Position : GL.Types.Singles.Vector3) is
   begin
      TR_Nodes.Clear;
      TR_Closest_Node := 0;
      TR_Farthest_Node := 0;
      TR_Camera_Position := Camera_Position;
   end Reset_Transparency_List;

   --  ----------------------------------------------------------------------------

end Transparency;
