
with Ada.Numerics;
with Game_Utils;
with Manifold;
with Tiles_Manager;

package body Prop_Renderer.Boulder is

   procedure Max_Min_XZ (Rp                         : Singles.Vector4; index : Int;
                         Max_X, Min_X, Max_Z, Min_Z : in out Single);

   --  -------------------------------------------------------------------------

   function Get_Prop_Height (Prop_Index         : Positive;
                             NW_World, SE_World : Singles.Vector3)
                             return Single is
      use Ada.Numerics;
      use GL.Types;
      use Singles;
      use Maths;
      use Single_Math_Functions;
      aProperty    : constant Prop_Renderer.Property_Data :=
                       Get_Property_Data (Prop_Index);
      S_I          : Positive := aProperty.Script_Index;
      aScript      : Prop_Renderer.Prop_Script := Get_Script_Data (S_I);
      Prop_Type    : constant Prop_Renderer.Property_Type := aScript.Script_Type;
      SS_I         : constant Positive := aScript.Smashed_Script_Index;
      Prop_Centre  : Vector3;
      Query_Centre : Vector3;
      Dist         : Vector3;
      Sqdist       : Single;
      Radius       : Single;
      F            : Single;
      Rp           : Singles.Vector4;
      P            : Singles.Vector4;
      Box_Point    : Vector2;
      Hole_Point   : Vector2;
      --        GL_Index     : constant array (Int range 1 .. 4) of GL.Index_Homogeneous
      --          := (GL.X, GL.Y, GL.Z, GL.W);
      Min_X        : Single := 0.0;
      Max_X        : Single := 0.0;
      Max_Y        : Single := 0.0;
      Min_Z        : Single := 0.0;
      Max_Z        : Single := 0.0;
      Min_Height   : constant Single := -100.0;
      Height       : Single := Min_Height;
      Continue     : Boolean := True;
   begin
      if aProperty.Was_Smashed and SS_I > 0 then
         S_I := SS_I;
         aScript := Get_Script_Data (S_I);
      end if;

      Prop_Centre := aProperty.Origin_World;
      Query_Centre := (SE_World - NW_World) / 2.0 + NW_World;
      Dist := Prop_Centre - Query_Centre;
      --  WARNING: this is a 2d distance for cylinder shape
      Sqdist := Dist (GL.X) ** 2 + Dist (GL.Z) ** 2;
      Radius := aScript.Radius;
      Height := aScript.Height;

      if Radius > 0.0 and Sqdist > Radius ** 2 then
         Height := Min_Height;
      elsif aScript.Has_Hole then
         for index in Int range 1 .. 4 loop
            Hole_Point := aScript.Hole_Points (index);
            P := (Hole_Point (GL.X), 0.0, Hole_Point (GL.Y), 1.0);
            Rp := aProperty.Model_Mat * P;
            Max_Min_XZ (Rp, index, Max_X, Min_X, Max_Z, Min_Z);
         end loop;

         Max_Y := aScript.Hole_Height + Prop_Centre (GL.Y);
         Continue := NW_World (GL.X) < Min_X or SE_World (GL.X) > Max_X or
           NW_World (GL.Y) > Max_Y or NW_World (GL.Z) < Min_Z or
           SE_World (GL.Z) > Max_Z;

         if not Continue then
            Height := Min_Height;
         end if;
      end if; --  has hole

      if Continue then
         case Prop_Type is
            when Boulder_Prop =>
               F := 0.5 * Pi * Sqdist / Radius ** 2;
               Height := Prop_Centre (GL.Y) + Radius * Cos (F);
               Continue := False;
            when Pillar_Prop =>
               if aProperty.Door = Closed_State then
                  Height := Height + Prop_Centre (GL.Y);
               else
                  Height := Prop_Centre (GL.Y);
               end if;
               Continue := False;
            when Door_Prop | Elevator_Prop | Box_Prop |
                 Mirror_Prop | Windlass_Prop | Big_Box_Prop =>
               if Prop_Type = Door_Prop and then
                 aProperty.Door = Open_State then
                  Height := Min_Height;
               else
                  Min_X := 0.0;
                  Max_X := 0.0;
                  Min_Z := 0.0;
                  Max_Z := 0.0;
                  for index in Int range 1 .. 4 loop
                     Box_Point := aScript.Box_Points (index);
                     P := (Box_Point (GL.X), 0.0, Box_Point (GL.Y), 1.0);
                     Rp := aProperty.Model_Mat * P;
                     Max_Min_XZ (Rp, index, Max_X, Min_X, Max_Z, Min_Z);
                  end loop;
               end if;

               if Min_X > Max (NW_World (GL.X), SE_World (GL.X)) or
                 Max_X < Min (NW_World (GL.X), SE_World (GL.X)) or
                 Min_Z > Max (NW_World (GL.Z), SE_World (GL.Z)) or
                 Max_Z < Min (NW_World (GL.Z), SE_World (GL.Z))then
                  Height := Min_Height;
               else
                  Height := Height + Prop_Centre (GL.Y);
               end if;
               Continue := False;

            when Bridge_Prop =>
               Height := aProperty.World_Pos (GL.Y);
               Continue := False;
            when Decap_Head_Prop | Tavern_Prop |Jav_Stand_Prop  =>
               Height := Min_Height;
               Continue := False;
            when others => null;
         end case;

         if Continue then
            if Radius <= 0.0 then
               Height := Min_Height;
            else
               Height := Prop_Centre (GL.Y) + aScript.Height;
            end if;
         end if;
      end if;

      return Height;

   end Get_Prop_Height;

   --  -------------------------------------------------------------------------

   function  Get_Prop_Height_Between_Bouldering
     (NW_World, SE_World : Singles.Vector3;
      Excluded_Property  : Positive) return Single is
      use Prop_Indices_Package;
      use Singles;
      use Maths;
      Prop_Indices       : Prop_Indices_List;
      Prop_Index         : Positive;
      aProperty          : Prop_Renderer.Property_Data;
      Prop_Type          : Prop_Renderer.Property_Type;
      aScript            : Prop_Renderer.Prop_Script;
      Nw_Map_U           : constant Int := Int (0.5 * (NW_World (GL.X) + 1.0));
      Nw_Map_V           : constant Int := Int (0.5 * (NW_World (GL.Z) + 1.0));
      Se_Map_U           : constant Int := Int (0.5 * (SE_World (GL.X) + 1.0));
      Se_Map_V           : constant Int := Int (0.5 * (SE_World (GL.Z) + 1.0));
      Left               : constant Int := Min_Int (Nw_Map_U, Se_Map_U);
      Right              : constant Int := Max_Int (Nw_Map_U, Se_Map_U);
      Top                : constant Int := Min_Int (Nw_Map_V, Se_Map_V);
      Bottom             : constant Int := Max_Int (Nw_Map_V, Se_Map_V);
      Head               : constant Single := Max (NW_World (GL.Y), SE_World (GL.Y));
      --  maybe go wider than the area given, and if dist - prop box/radius is
      --  bigger given area we consider it too
      Max_Prop_Tile_Rad  : constant Int := 2;
      Up_Bound           : constant Int := Max_Int (Top - Max_Prop_Tile_Rad, 0);
      Down_Bound         : constant Int := Min_Int (Bottom + Max_Prop_Tile_Rad,
                                                    Int (Manifold.Max_Tile_Cols - 1));
      Left_Bound         : constant Int := Max_Int (Left - Max_Prop_Tile_Rad, 0);
      Right_Bound        : constant Int := Min_Int (Right + Max_Prop_Tile_Rad,
                                                    Int (Manifold.Max_Tile_Cols - 1));
      Height             : Single := -100.0;
      New_Height         : Single;
      Prop_Radius        : Single;
      S_I                : Positive;
      SS_I               : Positive;
      Centre_Point       : Vector3;
      Dist               : Vector3;
      Sqdist             : Single;
      Prop_Sqrad         : Single;
      Zone_Rad           : Single;
      Zone_Sqrad         : Single;
   begin
      for v_index in Up_Bound .. Down_Bound loop
         for h_index in Left_Bound .. Right_Bound loop
            --  Check if radius and or centre is inside height range params
            Prop_Indices := Prop_Renderer.Get_Property_Indices
              (Integer (v_index), Integer (h_index));
            for index in Prop_Indices.First_Index ..
              Prop_Indices.Last_Index loop
               Prop_Index := Prop_Renderer.Get_Property_Index
                 (Integer (v_index), Integer (h_index), index);
               if Prop_Index /= Excluded_Property then
                  aProperty := Get_Property_Data (Prop_Index);
                  S_I := aProperty.Script_Index;
                  aScript := Get_Script_Data (S_I);
                  SS_I := aScript.Smashed_Script_Index;
                  if aProperty.Was_Smashed and SS_I > 0 then
                     S_I := SS_I;
                     aScript := Get_Script_Data (S_I);
                     Prop_Type := aScript.Script_Type;
                     if Prop_Type /= Pot_Prop and Prop_Type /= Door_Prop and
                       Prop_Type /= Pillar_Prop then
                        if Prop_Type /= Big_Box_Prop then
                           if v_index <= Top and v_index >= Bottom and
                             h_index <= Right_Bound and h_index >= Left_Bound then
                              if Head >= aProperty.World_Pos (GL.Y) then
                                 Prop_Radius := aScript.Radius;
                                 if Prop_Radius > 0.0 then
                                    Centre_Point := 0.5 * (NW_World + SE_World);
                                    Dist := Centre_Point - aProperty.Origin_World;
                                    Sqdist := Dist (Gl.X) ** 2 + Dist (Gl.Z) ** 2;
                                    Prop_Sqrad := Prop_Radius * Prop_Radius;
                                    -- also approximate box to a cylinder (circle)
                                    Zone_Rad := Centre_Point (Gl.X) - SE_World (Gl.X);
                                    Zone_Sqrad := Zone_Rad * Zone_Rad;
                                    if Sqdist <= Prop_Sqrad + Zone_Sqrad then
                                       New_Height := Get_Prop_Height (Prop_Index, NW_World, SE_World);
                                       Height := Max (Height, New_Height);
                                    end if;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;

      return Height;

   end Get_Prop_Height_Between_Bouldering;

   --  -------------------------------------------------------------------------

   procedure Max_Min_XZ (Rp                         : Singles.Vector4; Index : Int;
                         Max_X, Min_X, Max_Z, Min_Z : in out Single) is
      use Maths;
   begin
      if Index = 1 then
         Min_X := Rp (GL.X);
         Max_X := Min_X;
         Min_Z := Rp (GL.Z);
         Max_Z := Min_Z;
      else
         Min_X := Min (Rp (GL.X), Min_X);
         Max_X := Max (Rp (GL.X), Max_X);
         Min_Z := Min (Rp (GL.Z), Min_Z);
         Max_Z := Max (Rp (GL.Z), Max_Z);
      end if;
   end Max_Min_XZ;

   --  -------------------------------------------------------------------------

   procedure Update_Boulder (Prop_Index : Positive;
                             Script     : Prop_Renderer.Prop_Script;
                             Seconds    : Float) is

      use Singles;
      use Maths;
      use Tiles_Manager;
      Properties      : constant Prop_Renderer.Property_Data :=
                          Prop_Renderer.Get_Property_Data (Prop_Index);
      S_Seconds       : constant Single := Single (Seconds);
      Bounce_Factor   : constant Single := 0.5;
      Thresh          : constant Single := 0.1;
      Coeff_Fric      : constant Single := 0.2;
      Min_Speed       : constant Single := 0.05;
      Radius          : constant Single  := Script.Radius;
      --   if something (a wall/door/etc) was banged and should play sound etc
      Banged          : Boolean := False;
      --   update velocity in same way as characters
      Desired_Vel     : Vector3 := Properties.Velocity;
      Speed_Increase  : Single;
      Speed_Decrease  : Single;
      Next_Pos        : Vector3;
      Next_X          : Single;
      Next_Z          : Single;
      V_Sum           : Single;
      --   work out if can increase linear speed due to a ramp
      Current_U       : constant Int :=
                          Int (0.5 * (Properties.World_Pos (GL.X) + 1.0));
      Current_V       : constant Int :=
                          Int (0.5 * (Properties.World_Pos (GL.Z) + 1.0));
      Facing          : constant Character :=
                          Tiles_Manager.Get_Facing (Current_U, Current_V);
      Floor_Height    : Single;
      N               : Single;
      W               : Single;
      S               : Single;
      E               : Single;
      NW              : Vector3;
      SE              : Vector3;
      P_Height        : Single;
      Ultimate_Height : Single;
      Gap_To_Floor    : Single;
   begin
      if Properties.Is_On_Ground then
         if Manifold.Is_Ramp (Current_U, Current_V) then
            Speed_Increase := 7.5 * S_Seconds;  --  due to gravity
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
         Speed_Decrease := Coeff_Fric * S_Seconds;
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
         Desired_Vel (GL.Y) := Desired_Vel (GL.Y) - 10.0 * S_Seconds;
      end if;

      --  If stopped here, deactivate
      if Properties.Is_On_Ground and not
        Manifold.Is_Ramp (Current_U, Current_V) then
         V_Sum := abs (Desired_Vel (GL.X)) + abs (Desired_Vel (GL.Z));
         if V_Sum < Min_Speed then
            Desired_Vel := (0.0, 0.0, 0.0);
            --              Stop_Boulder_Sound (Properties.Boulder_Snd_Idx);
         else
            Next_Pos := Properties.World_Pos + S_Seconds * Desired_Vel;
            Next_X := Next_Pos (GL.X);
            Next_Z := Next_Pos (GL.Z);
            Floor_Height := Get_Tile_Height (Next_X, Next_Z, True, True);
            N := Get_Tile_Height (Next_X, Next_Z - Radius, True, True);
            W := Get_Tile_Height (Next_X - Radius, Next_Z, True, True);
            S := Get_Tile_Height (Next_X, Next_Z + Radius, True, True);
            E := Get_Tile_Height (Next_X + Radius, Next_Z, True, True);
            --  also bounce off props
            Nw := Next_Pos;  --  also bounce off props
            Nw (GL.X) := Nw (GL.X) + Radius;
            Nw (GL.Y) := Nw (GL.Y) - Radius;
            Nw (GL.Z) := Nw (GL.Z) - Radius;
            Nw (GL.X) := Nw (GL.X) + Radius;
            SE := Next_Pos;
            SE (GL.X) := SE (GL.X) + Radius;
            SE (GL.Z) := SE (GL.Z) + Radius;

            P_Height := Get_Prop_Height_Between_Bouldering (Nw, Se, Prop_Index);
            Ultimate_Height := Max (Floor_Height, P_Height);
            Gap_To_Floor := Next_Pos (GL.Y) - Radius - Ultimate_Height;
            Gap_To_Floor := Min (Gap_To_Floor, Next_Pos (GL.Y) - N);
            Gap_To_Floor := Min (Gap_To_Floor, Next_Pos (GL.Y) - W);
            Gap_To_Floor := Min (Gap_To_Floor, Next_Pos (GL.Y) - S);
            Gap_To_Floor := Min (Gap_To_Floor, Next_Pos (GL.Y) - E);
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