
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

   function Get_Player  (Player : Player_Index) return Sprite_Manager.Sprite is
   begin
      return Player_List (Player);
   end Get_Player;

   --  -------------------------------------------------------------------------

   function Get_Position  (Player : Player_Index) return Sprite_Manager.Point is
   begin
      return Sprite_Manager.Get_Position (Player_List (Player));
   end Get_Position;

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
      Set_Velocity (Player_List (Current_Player), 50.0);

   end Init_Players;

   --  -------------------------------------------------------------------------

   procedure Jump (Player : Player_Index;
                   Status : Sprite_Manager.Sprite_Status)  is
   begin
      Sprite_Manager.Jump (Player_List (Player), Status);
   end Jump;

   --  -------------------------------------------------------------------------

   procedure Render_Players is
   begin
      for index in Player_Index'Range loop
         Sprite_Manager.Render (Player_List (index));
      end loop;
   end Render_Players;

   --  -------------------------------------------------------------------------

   procedure Set_Active (Player : Player_Index; State : Boolean) is
   begin
      Sprite_Manager.Set_Active (Player_List (Player), State);
   end Set_Active;

   --  -------------------------------------------------------------------------

   procedure Set_Current_Player (Player : Player_Index) is
   begin
      Current_Player := Player;
   end Set_Current_Player;

   --  -------------------------------------------------------------------------

   procedure Set_Position (Player   : Player_Index;
                           Position : Sprite_Manager.Point) is
   begin
      Sprite_Manager.Set_Position (Player_List (Player), Position);
   end Set_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Visible (Player : Player_Index; State : Boolean) is
   begin
      Sprite_Manager.Set_Visible (Player_List (Player), State);
   end Set_Visible;

   --  -------------------------------------------------------------------------

   procedure Set_Velocity (Player : Player_Index; Step : Float) is
   begin
      Sprite_Manager.Set_Velocity (Player_List (Player), Step);
   end Set_Velocity;

   --  -------------------------------------------------------------------------

   procedure Update (Delta_Time : Float) is
   begin
      for index in Player_Index'Range loop
         Sprite_Manager.Update (Player_List (index), Delta_Time);
      end loop;
   end Update;

   --  -------------------------------------------------------------------------

end Player_Manager;
