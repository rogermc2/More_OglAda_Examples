
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with Batch_Manager;
with Game_Utils;
with Tiles_Manager;

package body Character_Controller is

   type Weapon_Type is (Sword_Wt, Missile_Wt, --  used for javelin, arrow, green fireballs
                        Hammer_Wt, Skull_Wt, Teleport_Wt, Pillar_Wt, Boulder_Wt,
                        Fall_Wt, Na_Wt); --  everything else, n/a.
   pragma Ordered (Weapon_Type);

   --      type Integer_Array is array (Integer range <>) of Integer;
   type Character_Data is record
      Script_File   : Unbounded_String := To_Unbounded_String ("");
      U             : GL.Types.Int := 0;
      V             : GL.Types.Int := 0;
      H             : Float := 0.0;
   end record;

   Characters : Character_List;

   --      Portal_Fadeout_Started  : Boolean := False;
   Characters_To_Reserve   : constant Integer := 256;
   Characters_Allocd_Count : Integer := Characters_To_Reserve;
   Character_Count         : Integer := Characters_To_Reserve;
   --      Specs_Allocd_Count      : Integer := 0;
   --      Specs_Count             : Integer := 0;
   --      Gold_Current            : constant Integer := 0;
   --      Kills_Current           : Integer := 0;
   --      Kills_Max               : Integer := 0;

   --      function Is_Character_Valid (Char_Index : Integer) return Boolean;
   --      function Is_Spec_Valid (Spec_Index : Integer) return Boolean;
   procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character);

   --  -------------------------------------------------------------------------

   procedure Create_Character (Source       : Character_Data;
                               theCharacter : in out Barbarian_Character) is
   begin
      if Source.Script_File = "" then
         raise Character_Controller_Exception with
           "Character_Controller.Create_Character, no script file name.";
      end if;

      if Tiles_Manager.Is_Tile_Valid (Source.U, Source.V) then
         Game_Utils.Game_Log ("Character_Controller.Create_Character creating character from " &
                                To_String (Source.Script_File));
         Set_Character_Defaults (theCharacter);
         theCharacter.Heading_Deg := Source.H;
         theCharacter.Map_X := Source.U;
         theCharacter.Map_Y := Source.V;
         theCharacter.Specs_Index :=
           Specs_Manager.Get_Script_Index (To_String (Source.Script_File));
         Character_Count := Character_Count + 1;
         Game_Utils.Game_Log ("Character_Controller.Create_Character character created from " &
                                To_String (Source.Script_File));
      else
         raise Character_Controller_Exception with
           "Character_Controller.Create_Character, invalid tile siza" &
           Int'Image (Source.U) & "x" & Int'Image (Source.V);
      end if;
   end Create_Character;

   --  -------------------------------------------------------------------------

   function Damage_All_Near
     (Self_Id        : Positive; World_Pos : Singles.Vector3; Damage_Range : Single;
      Damage         : Int;
      Throw_Back_Mps : Single;  Exclude_Id : Positive; Weapon : Weapon_Type) return Natural is
      use Maths;
      Map_U               : Int := Int (0.5 * (World_Pos (Gl.X) + 1.0));
      Map_V               : Int := Int (0.5 * (World_Pos (Gl.Z) + 1.0));
      Left                : Int := Maths.Max_Int (0, Map_U - 1);
      Right               : Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1, Map_U + 1);
      Up                  : Int := Maths.Max_Int (0, Map_V - 1);
      Down                : Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1, Map_V + 1);
      Last_Character_Hit  : Int := -1;

   begin
      return 0;

   end Damage_All_Near;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      null;  --  Nothing to do
   end Init;

   --  -------------------------------------------------------------------------

   --      function Is_Character_Valid (Char_Index : Integer) return Boolean is
   --      begin
   --          return Char_Index >= 0 and Char_Index < Characters_Allocd_Count;
   --      end Is_Character_Valid;

   --  -------------------------------------------------------------------------

   --      function Is_Spec_Valid (Spec_Index : Integer) return Boolean is
   --      begin
   --          return Spec_Index >= 0 and Spec_Index < Specs_Allocd_Count;
   --      end Is_Spec_Valid;

   --  -------------------------------------------------------------------------
   --  read characters from an already open file stream
   procedure Load_Characters (Input_File : File_Type; Editor_Mode : Boolean) is
      use Ada.Strings.Fixed;
      Pos1           : Integer;
      Pos2           : Integer;
      Num_Characters : Integer := 0;
      Field          : Character_Data;
      aCharacter     : Barbarian_Character;
   begin
      Game_Utils.Game_Log
        ("Character_Controller.Load_Characters, loading characters.");
      --          Portal_Fadeout_Started := False;
      Specs_Manager.Clear_Specs;

      if Characters.Is_Empty then
         Characters_Allocd_Count := Characters_To_Reserve;
      else
         Game_Utils.Game_Log
           ("Character_Controller.Load_Characters, " &
              Ada.Containers.Count_Type'Image (Characters.Length) & "/" &
              Integer'Image (Characters_To_Reserve) & "were used last time.");
      end if;
      Characters.Clear;
      --          Kills_Current := 0;
      --          Kills_Max := 0;

      if not Editor_Mode then
         Put_Line ("not Editor_Mode");
         --              GUI.Set_GUI_Gold (Gold_Current);
         --              GUI.Set_GUI_Kills (Kills_Current);
         --              GUI.Set_GUI_Javalin_Ammo (0);
      end if;

      declare
         aLine    : constant String := Get_Line (Input_File);
         L_Length : constant Integer := aLine'Length;
      begin
         if aLine (1 .. 6) /= "chars " then
            raise Character_Controller_Exception with
              "Character_Controller.Load_Characters, invalid Character header: "
              & aLine;
         end if;
         Pos1 := Index (aLine (7 .. L_Length), " ");
         Num_Characters := Integer'Value (aLine (7 .. Pos1 - 1));
      end;

      for count in 1 .. Num_Characters loop
         declare
            aLine    : constant String := Get_Line (Input_File);
            L_Length : constant Integer := aLine'Length;
         begin
            Pos1 := Index (aLine, " ");
            Field.Script_File := To_Unbounded_String (aLine (1 .. Pos1 - 1));
            Pos2 := Index (aLine (Pos1 + 1 .. L_Length), ",");
            Field.U := Int'Value (aLine (Pos1 + 1 .. Pos2 - 1));
            Pos1 := Index (aLine (Pos2 + 1 .. L_Length), " ");
            Field.V := Int'Value (aLine (Pos2 + 1 .. Pos1 - 1));
            Field.H := Float (Int'Value (aLine (Pos1 + 1 .. L_Length)));
            Create_Character (Field, aCharacter);
         end;
      end loop;
      Game_Utils.Game_Log
        ("Character_Controller.Load_Characters, all characters loaded.");

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Character_Controller.Load_Characters!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Characters;

   --  ----------------------------------------------------------------------------

   procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character) is
   begin
      aCharacter.Destination_Tile_X := -1;
      aCharacter.Destination_Tile_Y := -1;
      aCharacter.Is_Alive := True;
      aCharacter.Is_On_Ground := True;
   end Set_Character_Defaults;

   --  -------------------------------------------------------------------------

   function Update_Characters (Seconds : Float) return Boolean is
   begin
      return False;
   end Update_Characters;

   --  -------------------------------------------------------------------------

end Character_Controller;
