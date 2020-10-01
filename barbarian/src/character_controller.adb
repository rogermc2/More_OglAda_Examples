
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Tiles_Manager;

package body Character_Controller is

    --      type Integer_Array is array (Integer range <>) of Integer;
    type Character_Data is record
        Script_File   : Unbounded_String := To_Unbounded_String ("");
        U      : GL.Types.Int := 0;
        V      : GL.Types.Int := 0;
        H      : Float := 0.0;
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

    procedure Create_Character (Source : Character_Data;
                                theCharacter : in out Barbarian_Character) is
    begin
        if Source.Script_File = "" then
            raise Character_Controller_Exception with
              "Character_Controller.Create_Character, no script file name.";
        end if;
        if Tiles_Manager.Is_Tile_Valid (Source.U, Source.V) then
            Game_Utils.Game_Log ("Creating character from " &
                                   To_String (Source.Script_File));
            if Character_Count >= Characters_Allocd_Count then
                Game_Utils.Game_Log ("WARNING: realloc characters.");
                Characters_Allocd_Count := Character_Count + 64;
            end if;

            Set_Character_Defaults (theCharacter);
            theCharacter.Heading_Deg := Source.H;
            theCharacter.Map_X := Source.U;
            theCharacter.Map_Y := Source.V;
            theCharacter.Specs_Index :=
              Specs_Manager.Get_Script_Index (To_String (Source.Script_File));
            Character_Count := Character_Count + 1;
        else
            raise Character_Controller_Exception with
              "Character_Controller.Create_Character, invalid tile siza" &
              Int'Image (Source.U) & "x" & Int'Image (Source.V);
        end if;
    end Create_Character;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        null;
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
    procedure Load_Characters (Input_Stream : Stream_IO.Stream_Access;
                               Editor_Mode : Boolean) is
        type Character_Header is record
            Label     : String (1 .. 6) := "      ";
            Num_Characters : Integer := 0;
            Comment   : Unbounded_String := To_Unbounded_String ("");
        end record;

        Header     : Character_Header;
        Field      : Character_Data;
        aCharacter : Barbarian_Character;
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

        Character_Header'Read (Input_Stream, Header);
        if Header.Label /= "chars " then
            raise Character_Controller_Exception with
              "Character_Controller.Load_Characters, invalid Character header: "
              & Header.Label;
        end if;

        for count in 1 .. Header.Num_Characters loop
            Character_Data'Read (Input_Stream, Field);
            Create_Character (Field, aCharacter);
        end loop;

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
