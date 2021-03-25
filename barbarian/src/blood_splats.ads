
with GL.Types;

package Blood_Splats is

   procedure Clear_Splats;
   procedure Init;
   procedure Render_Splats;
   procedure Set_Ambient_Light_Level (Level : GL.Types.Singles.Vector3);
   procedure Splat_Event (Position : GL.Types.Singles.Vector3; Radius : Float);
   procedure Use_Splats_Shader;

end Blood_Splats;
