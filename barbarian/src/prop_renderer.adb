
with Game_Utils;
package body Prop_Renderer is

    --  Animation and rendering
	Model_Matrix     : Singles.Matrix4 := (others => (others => 0.0));
	Current_Bone_Transforms : Singles.Matrix4_Array (1 .. Mesh_Loader.Max_Bones);
	Anim_Duration     : Integer := 0;
	Anim_Elapsed_Time : Integer := 0;
	Sprite_Duration   : Integer := 0;
	Delay_Countdown   : Integer := 0;
	--  Hack to stop decap head bouncing when stuck
	Bounce_Count      : Integer := 0;

   --  -------------------------------------------------------------------------

   procedure Init_Prop_Renderer is
   begin
      Game_Utils.Game_Log ("---INIT PROPS---");


        Game_Utils.Game_Log ("---PROPS INITIALIZED---");
   end Init_Prop_Renderer;

   --  -------------------------------------------------------------------------

   function Update_Props (Seconds : Float) return Boolean is
   begin
        return False;
   end Update_Props;

   --  -------------------------------------------------------------------------

end Prop_Renderer;
