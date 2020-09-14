
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Texture_Manager;

with Game_Utils;

package body Specs_Manager is

    Max_Anim_Frames   : constant GL.Types.Int := 32;
    Max_Attack_Events : constant GL.Types.Int := 32;

    Specs                   : Specs_List;
    Use_Character_Mipmaps   : Boolean := True;

   procedure Set_Specs_Defaults (aSpec : in out Spec_Data);

    --  ------------------------------------------------------------------------

    procedure Clear_Specs is
    begin
    	Specs.Clear;
    end Clear_Specs;

    --  -------------------------------------------------------------------------

    function Get_Script_Index (File_Name : String) return Integer is
        use Specs_Package;
        Curs       : Cursor := Specs.First;
        Data       : Spec_Data;
        Found      : Boolean := False;
        Spec_Index : Integer := 0;
    begin
        while Has_Element (Curs) and not Found loop
            Data := Element (Curs);
            Found := Data.File_Name = File_Name;
            if Found then
                Spec_Index := To_Index (Curs);
            end if;
            Next  (Curs);
        end loop;

        if not Found then
            Game_Utils.Game_Log
              ("Character_Controller.Get_Script_Index loading specs file: " &
                 File_Name);
            Found := Load_Specs_File (File_Name);
        end if;

        return Spec_Index;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Character_Controller.Get_Script_Index!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return -1;
    end Get_Script_Index;

    --  -------------------------------------------------------------------------

    function Load_Specs_File (File_Name : String) return Boolean is
        Path          : String := "characters/" & File_Name;
        Input_File    : File_Type;
        Specs_Count   : constant Integer := Integer (Specs.Length);
        Attack_Sound  : Integer := 0;
        Atlas_Index   : Integer := 0;
        Anim_Num      : Int := 0;
        Seconds       : Float := 0.0;
        AFC           : Int := 0;
        theSpec       : Spec_Data;
        Result        : Boolean := False;
    begin
        Open (Input_File, In_File, Path);
        theSpec.File_Name := To_Unbounded_String (File_Name);
        Set_Specs_Defaults (theSpec);

        while not End_Of_File (Input_File) loop
            declare
                aLine    : constant String := Get_Line (Input_File);
                UB       : Unbounded_String := To_Unbounded_String (aLine);
                S_Length : constant Natural := aLine'Length;
                Pos1     : Natural := Index (UB, " ");
                Pos2     : Natural;
                Pos_M1   : Natural := Pos1 - 1;
                Pos_P1   : Natural := Pos1 + 1;
            begin
                if  aLine'Length < 2 or else aLine (1 .. 1) = "#" or else
                  aLine (1 .. 1) = "/"  then
                    null;
                elsif aLine (1 .. Pos_M1) = "name" then
                    theSpec.Name :=
                      To_Unbounded_String (aLine (Pos_P1 .. S_Length));
                elsif aLine (1 .. Pos_M1) = "team_id:" then
                    theSpec.Team_ID :=
                      Integer'Value (aLine (Pos_P1 .. S_Length));
                elsif aLine (1 .. Pos_M1) = "sprite_map_diffuse" then
                    Path := "textures/" & aLine (Pos_P1 .. S_Length);
                    Texture_Manager.Load_Image_To_Texture
                      (Path, theSpec.Atlas_Diffuse_ID, Use_Character_Mipmaps, True);
                    Game_Utils.Game_Log ("Loaded sprite diffuse map " & Path);
                elsif aLine (1 .. Pos_M1) = "sprite_map_specular" then
                    Path := "textures/" & aLine (Pos_P1 .. S_Length);
                    Texture_Manager.Load_Image_To_Texture
                      (Path, theSpec.Atlas_Diffuse_ID, Use_Character_Mipmaps, True);
                    Game_Utils.Game_Log ("Loaded sprite specular map " & Path);
                elsif aLine (1 .. Pos_M1) = "sprite_map_rows" then
                    theSpec.Atlas_Rows :=
                      Integer'Value (aLine (Pos_P1 .. S_Length));
                elsif aLine (1 .. Pos_M1) = "sprite_map_cols" then
                    theSpec.Atlas_Cols :=
                      Integer'Value (aLine (Pos_P1 .. S_Length));
                elsif aLine (1 .. Pos_M1) = "add_anim_frame" then
                    UB := To_Unbounded_String (Slice (UB, Pos_P1, S_Length));
                    Pos2 := Index (UB, " ");
                    Anim_Num := Int'Value (aLine (Pos_P1 .. Pos2 - 1));
                    Result := Anim_Num >= 0 and Anim_Num < Max_Animations;
                    if Result then
                        --  Skip "atlas_index:"
                        UB := To_Unbounded_String (Slice (UB, Pos2 + 1, S_Length));
                        Pos1 := Index (UB, " ") + 1;
                        UB := To_Unbounded_String (Slice (UB, Pos1, S_Length));
                        Pos2 := Index (UB, " ");
                        Atlas_Index :=  Integer'Value (aLine (Pos1 .. Pos2 - 1));
                        --  Skip "seconds:"
                        UB := To_Unbounded_String (Slice (UB, Pos2 + 1, S_Length));
                        Pos1 := Index (UB, " ") + 1;
                        UB := To_Unbounded_String (Slice (UB, Pos1, S_Length));
                        Pos2 := Index (UB, " ");
                        Seconds :=  Float'Value (aLine (Pos_P1 .. Pos2 - 1));
                        AFC := theSpec.Animation_Frame_Count (Anim_Num);
                        Result := AFC < Max_Animations;
                        if Result then
                            null;
                        else
                            raise Specs_Exception with
                           "Specs_Manager.Load_Specs_File too many animation frames.";
                        end if;
                    else
                        raise Specs_Exception with
                          "Specs_Manager.Load_Specs_File invalid animation index: "
                          & aLine (Pos_P1 .. Pos2 - 1);
                    end if;
                elsif aLine (1 .. Pos_M1) = "sprite_map_cols" then
                    theSpec.Atlas_Cols :=
                      Integer'Value (aLine (Pos_P1 .. S_Length));
                end if;
            end;  --  declare block
        end loop;

        Close (Input_File);
        return Result;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Specs_Manager.Load_Specs_File!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return False;
    end Load_Specs_File;

    --  ----------------------------------------------------------------------------

   procedure Set_Specs_Defaults (aSpec : in out Spec_Data) is
    begin
        aSpec.Move_Speed_MPS := 4.0;
        aSpec.Height_Metre := 1.8;
        aSpec.Width_Radius := 0.5;
        aSpec.Initial_Health := 100;
        aSpec.Decapitated_Head_Prop_Script := -1;
        aSpec.Land_Move := True;
        aSpec.Tx_On_Death := -1;
    end Set_Specs_Defaults;

    --  -------------------------------------------------------------------------

end Specs_Manager;
