
package Player_Manager is

   type Player_Index is (Robot_Left, Robot_Right,
                        Robot_Left_Strip, Robot_Right_Strip);

   function Get_Current_Player return Player_Index;
   procedure Init_Players;
   procedure Set_Current_Player (Player : Player_Index);


end Player_Manager;
