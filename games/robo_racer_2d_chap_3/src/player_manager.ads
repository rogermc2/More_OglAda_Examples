
with Sprite_Manager;

package Player_Manager is

   type Player_Index is (Robot_Left, Robot_Right,
                        Robot_Left_Strip, Robot_Right_Strip);

   function Get_Current_Player return Player_Index;
   function Get_Player  (Player : Player_Index) return Sprite_Manager.Sprite;
   procedure Init_Players;
   procedure Render_Players;
   procedure Set_Current_Player (Player : Player_Index);
   procedure Update (Delta_Time : Float);

end Player_Manager;
