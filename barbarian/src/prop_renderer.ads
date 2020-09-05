
with GL.Types; use GL.Types;

with Mesh_Loader;

package Prop_Renderer is
    --  Animation and rendering
	Model_Matrix     : Singles.Matrix4 := (others => (others => 0.0));
	Current_Bone_Transforms : Singles.Matrix4_Array (1 .. Mesh_Loader.Max_Bones);
	Anim_Duration     : Integer := 0;
	Anim_Elapsed_Time : Integer := 0;
	Sprite_Duration   : Integer := 0;
	Delay_Countdown   : Integer := 0;
	--  Hack to stop decap head bouncing when stuck
	Bounce_Count      : Integer := 0;

   function Init_Prop_Renderer return Boolean;
   function Update_Props (Seconds : Float) return Boolean;

end Prop_Renderer;
