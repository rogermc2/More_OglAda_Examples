
with Game_Utils;
with Manifold;
with Tiles_Manager;

package body Prop_Renderer.Boulder is

   --  -------------------------------------------------------------------------

   procedure Update_Boulder (Property : Prop_Renderer.Property_Data;
                             Script     : Prop_Renderer.Prop_Script;
                             Seconds : Float) is
      use Maths;
      Bounce_Factor  : constant Single := 0.5;
      Thresh         : constant Single := 0.1;
      Coeff_Fric     : constant Single := 0.2;
      Min_Speed      : constant Single := 0.05;
      Radius         : Single  := Script.Radius;
      --   if something (a wall/door/etc) was banged and should play sound etc
      Banged         : Boolean := False;
      --   update velocity in same way as characters
      Desired_Vel    : Singles.Vector3 := Property.Velocity;
      Speed_Increase : Single;
      Speed_Decrease : Single;
      V_Sum          : Single;
      --   work out if can increase linear speed due to a ramp
      Current_U      : constant Int :=
                         Int (0.5 * (Property.World_Pos (GL.X) + 1.0));
      Current_V      : constant Int :=
                         Int (0.5 * (Property.World_Pos (GL.Z) + 1.0));
      Facing         : constant Character :=
                         Tiles_Manager.Get_Facing (Current_U, Current_V);

   begin
      if Property.Is_On_Ground then
         if Manifold.Is_Ramp (Current_U, Current_V) then
            Speed_Increase := Single (7.5 * Seconds);  --  due to gravity
            Desired_Vel (GL.Y) := Desired_Vel (GL.Y) - Speed_Increase;
            if Facing = 'N' then
               Desired_Vel (GL.Z) := Desired_Vel (GL.Z) + Speed_Increase;
            elsif Facing = 'S' then
               Desired_Vel (GL.Z) := Desired_Vel (GL.Z) - Speed_Increase;
            elsif Facing = 'W' then
               Desired_Vel (GL.X) := Desired_Vel (GL.X) + Speed_Increase;
            else
               Desired_Vel (GL.X) := Desired_Vel (GL.X) - Speed_Increase;
            end if;
         end if;
         --  Acccount for friction
         Speed_Decrease := Coeff_Fric * Single (Seconds);
         if Desired_Vel (GL.X) > 0.0 then
            Desired_Vel (GL.X) := Max (Desired_Vel (GL.X) - Speed_Decrease , 0.0);
         else
            Desired_Vel (GL.X) := Min (Desired_Vel (GL.X) + Speed_Decrease , 0.0);
         end if;
         if Desired_Vel (GL.Z) > 0.0 then
            Desired_Vel (GL.Z) := Max (Desired_Vel (GL.Z) - Speed_Decrease , 0.0);
         else
            Desired_Vel (GL.Z) := Min (Desired_Vel (GL.Z) + Speed_Decrease , 0.0);
         end if;
      else
         Desired_Vel (GL.Y) := Desired_Vel (GL.Y) - Single (10.0 * Seconds);
      end if;

      --  If stopped here, deactivate
      if Property.Is_On_Ground and not
        Manifold.Is_Ramp (Current_U, Current_V) then
         V_Sum := abs (Desired_Vel (GL.X)) + abs (Desired_Vel (GL.Z));
         if V_Sum < Min_Speed then
            Desired_Vel := (0.0, 0.0, 0.0);
         end if;
      end if;

   end Update_Boulder;

   --  -------------------------------------------------------------------------

   procedure Update_Dart_Trap (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Dart_Trap;

   --  -------------------------------------------------------------------------

   procedure Update_Door (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Door;

   --  -------------------------------------------------------------------------

   procedure Update_Elevator (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Elevator;

   --  -------------------------------------------------------------------------

   procedure Update_Pillar (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Pillar;

   --  -------------------------------------------------------------------------

   procedure Update_Decap_Head (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Decap_Head;

   --  -------------------------------------------------------------------------

   procedure Update_Windlass (Prop_Index : Positive; Seconds : Float) is
   begin
      null;
   end Update_Windlass;

   --  -------------------------------------------------------------------------

end Prop_Renderer.Boulder;
