
with Sprite_Manager;

package body Player_Manager is

   Player_List    : array (Player_Index range Player_Index'Range) of
     Sprite_Manager.Sprite;
   Current_Player : Player_Index := Robot_Right;

   --  -------------------------------------------------------------------------

   function Get_Current_Player return Player_Index is
   begin
      return Current_Player;
   end Get_Current_Player;

   --  -------------------------------------------------------------------------

   procedure Init_Players is
      use Sprite_Manager;
   begin

      Set_Frame_Size (Player_List (Robot_Right), 100.0, 125.0);
      Set_Number_Of_Frames (Player_List (Robot_Right), 4);
      Set_Position (Player_List (Robot_Right), 10.0, 50.0);
      Add_Texture (Player_List (Robot_Right), "src/resources/robot_right_00.png");
      Add_Texture (Player_List (Robot_Right), "src/resources/robot_right_01.png");
      Add_Texture (Player_List (Robot_Right), "src/resources/robot_right_02.png");
      Add_Texture (Player_List (Robot_Right), "src/resources/robot_right_03.png");

      Set_Frame_Size (Player_List (Robot_Left), 100.0, 125.0);
      Set_Number_Of_Frames (Player_List (Robot_Left), 4);
      Set_Position (Player_List (Robot_Left), 500.0, 50.0);
      Add_Texture (Player_List (Robot_Left), "src/resources/robot_left_00.png");
      Add_Texture (Player_List (Robot_Left), "src/resources/robot_left_01.png");
      Add_Texture (Player_List (Robot_Left), "src/resources/robot_left_02.png");
      Add_Texture (Player_List (Robot_Left), "src/resources/robot_left_03.png");

      Set_Frame_Size (Player_List (Robot_Right_Strip), 125.0, 100.0);
      Set_Number_Of_Frames (Player_List (Robot_Right_Strip), 4);
      Set_Position (Player_List (Robot_Right_Strip), 0.0, 50.0);
      Add_Texture (Player_List (Robot_Right_Strip), "src/resources/robot_right_strip.png");

      Set_Frame_Size (Player_List (Robot_Left_Strip), 125.0, 100.0);
      Set_Number_Of_Frames (Player_List (Robot_Left_Strip), 4);
      Set_Position (Player_List (Robot_Left_Strip), 0.0, 50.0);
      Add_Texture (Player_List (Robot_Left_Strip), "src/resources/robot_left_strip.png");

      Set_Visible (Player_List (Robot_Right), True);
      Set_Active (Player_List (Robot_Right), True);
      Set_Velocity (Player_List (Robot_Right), 50.0);

      Current_Player := Robot_Right;

   end Init_Players;

   --  -------------------------------------------------------------------------

   procedure Set_Current_Player (Player : Player_Index) is
   begin
      Current_Player := Player;
   end Set_Current_Player;

   --  -------------------------------------------------------------------------

end Player_Manager;
