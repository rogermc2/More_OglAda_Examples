
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with GUI;
with Manifold;

package body Character_Controller is

--      type Integer_Array is array (Integer range <>) of Integer;
    type Character_Data is record
       Script_File   : Unbounded_String := To_Unbounded_String ("");
       U      : GL.Types.Int := 0;
       V      : GL.Types.Int := 0;
       H      : Float := 0.0;
    end record;

    Characters : Character_List;
    Specs      : Specs_List;

    Portal_Fadeout_Started  : Boolean := False;
    Characters_To_Reserve   : constant Integer := 256;
    Characters_Allocd_Count : Integer := Characters_To_Reserve;
    Character_Count         : Integer := Characters_To_Reserve;
    Gold_Current            : Integer := 0;
    Kills_Current           : Integer := 0;
    Kills_Max               : Integer := 0;

    procedure Set_Character_Defaults (Character_Index : Integer);

    --  -------------------------------------------------------------------------

    procedure Create_Character (theCharacter : Character_Data) is
        use GL.Types;
    begin
        if theCharacter.Script_File = "" then
            raise Character_Controller_Exception with
            "Character_Controller.Create_Character, no script file name.";
        end if;
        if Manifold.Is_Tile_Valid (theCharacter.U, theCharacter.V) then
            Game_Utils.Game_Log ("Creating character from " &
                                  To_String (theCharacter.Script_File));
            if Character_Count >= Characters_Allocd_Count then
                Game_Utils.Game_Log ("WARNING: realloc characters.");
                Characters_Allocd_Count := Character_Count + 64;
            end if;

        else
            raise Character_Controller_Exception with
            "Character_Controller.Create_Character, invalid tile siza" &
            Int'Image (theCharacter.U) & "x" & Int'Image (theCharacter.V);
        end if;
    end Create_Character;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        null;
    end Init;

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
        aCharacter : Character_Data;
    begin
       Game_Utils.Game_Log
              ("Character_Controller.Load_Characters, loading characters.");
        Portal_Fadeout_Started := False;
        Specs.Clear;

        if Characters.Is_Empty then
            Characters_Allocd_Count := Characters_To_Reserve;
        else
            Game_Utils.Game_Log
              ("Character_Controller.Load_Characters, " &
                Ada.Containers.Count_Type'Image (Characters.Length) & "/" &
                Integer'Image (Characters_To_Reserve) & "were used last time.");
        end if;
        Characters.Clear;
        Kills_Current := 0;
        Kills_Max := 0;

        if not Editor_Mode then
            GUI.Set_GUI_Gold (Gold_Current);
            GUI.Set_GUI_Kills (Kills_Current);
            GUI.Set_GUI_Javalin_Ammo (0);
        end if;

        Character_Header'Read (Input_Stream, Header);
        if Header.Label /= "chars " then
            raise Character_Controller_Exception with
            "Character_Controller.Load_Characters, invalid Character header: "
            & Header.Label;
        end if;

        for count in 1 .. Header.Num_Characters loop
            Character_Data'Read (Input_Stream, aCharacter);
            Create_Character (aCharacter);
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Properties_Manager.Load_Characters!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Characters;

    --  ----------------------------------------------------------------------------

    procedure Set_Character_Defaults (Character_Index : Integer) is
    begin
        null;
    end Set_Character_Defaults;

    --  -------------------------------------------------------------------------
    function Update_Characters (Seconds : Float) return Boolean is
    begin
        return False;
    end Update_Characters;

    --  -------------------------------------------------------------------------

end Character_Controller;
