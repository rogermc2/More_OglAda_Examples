
with GL.Objects.Textures;
with GL.Types;

with Camera;

package Main_Loop.Game_Support is

   function Cheat_Check_1 return Boolean;
   function Check_Victory_Defeat return Boolean;
   procedure Player_1_View (Window     : in out Input_Callback.Barbarian_Window;
                            Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                            Ramp_Spec_Tex : GL.Objects.Textures.Texture;
                            Delta_Time : Float; Dump_Video : Boolean;
                            Save_Screenshot : Boolean);
   procedure Unload_Level;
   procedure Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                            Curr_Frame_Time_Accum_Ms : in out Float;
                            Avg_Frames_Count, Curr_Frames_Count  : in out Integer);

end Main_Loop.Game_Support;
