
with Sprite_Manager;

package Player_Manager is

   type Player_Index is (Robot_Left, Robot_Right,
                         Robot_Left_Strip, Robot_Right_Strip);

   function Get_Current_Player return Player_Index;
   function Get_Collision_Rectangle  (Player : Player_Index)
                                      return Sprite_Manager.Rectangle;
   function Get_Player (Player : Player_Index) return Sprite_Manager.Sprite;
   function Get_Position (Player : Player_Index) return Sprite_Manager.Point;
   function Get_Size (Player : Player_Index) return Sprite_Manager.Object_Size;
   procedure Init_Players;
   procedure Jump (Player : Player_Index; Status : Sprite_Manager.Sprite_Status);
   procedure Render_Players;
   procedure Set_Active (Player : Player_Index; State : Boolean);
   procedure Set_Collision (Player : Player_Index;
                            Rect : Sprite_Manager.Rectangle);
   procedure Set_Current_Player (Player : Player_Index);
   procedure Set_Position (Player   : Player_Index;
                           Position : Sprite_Manager.Point);
   procedure Set_Visible (Player : Player_Index; State : Boolean);
   procedure Set_Velocity (Player : Player_Index; Step : Float);
   procedure Update (Delta_Time : Float);

end Player_Manager;
