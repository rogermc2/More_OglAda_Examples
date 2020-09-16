
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Texture_Manager;

with Game_Utils;

package body Specs_Manager is

--      Max_Anim_Frames   : constant GL.Types.Int := 32;

    Specs                   : Specs_List;
    Use_Character_Mipmaps   : constant Boolean := True;

    procedure Set_Specs_Defaults (aSpec : in out Spec_Data);

    --  ------------------------------------------------------------------------

    procedure Add_Animation_Frame (aLine : String; theSpec : in out Spec_Data) is
        use Ada.Strings;
        Atlas_Index : Integer := 0;
        Anim_Num    : Int := 0;
        Seconds     : Float := 0.0;
        AFC         : Int := 0;
        Pos1        : Natural := Fixed.Index (aLine, " ");
        Pos2        : Natural;
        Pos_P1      : constant Natural := Pos1 + 1;
        anAnim      : Anim_Frame;
        Anim_1D     : Anim_Frame_List;
        Anim_2D     : Anim_Frame_Array;
    begin
        Pos2 := Fixed.Index (aLine, " ");
        Anim_Num := Int'Value (aLine (Pos_P1 .. Pos2 - 1));
        if Anim_Num >= 0 and Anim_Num < Max_Animations then
            --  Skip "atlas_index:"
            Pos1 := Fixed.Index (aLine, " ") + 1;
            Pos2 := Fixed.Index (aLine, " ");
            Atlas_Index :=  Integer'Value (aLine (Pos1 .. Pos2 - 1));
            --  Skip "seconds:"
            Pos1 := Fixed.Index (aLine, " ") + 1;
            Pos2 := Fixed.Index (aLine, " ");
            Seconds :=  Float'Value (aLine (Pos_P1 .. Pos2 - 1));
            AFC := theSpec.Animation_Frame_Count (Anim_Num);

            if AFC < Max_Animations then
                anAnim := (Int (Atlas_Index), Single (Seconds));
                Anim_1D.Replace_Element (Integer (Anim_Num), anAnim);
                Anim_2D.Replace_Element (Integer (Anim_Num), Anim_1D);
                theSpec.Animations := Anim_2D;
                theSpec.Animation_Frame_Count (Anim_Num) :=
                  theSpec.Animation_Frame_Count (Anim_Num) + 1;
            else
                raise Specs_Exception with
                  "Specs_Manager.Add_Animation_Frame too many animation frames.";
            end if;
        else
            raise Specs_Exception with
              "Specs_Manager.Add_Animation_Frame invalid animation index: "
              & aLine (Pos_P1 .. Pos2 - 1);
        end if;
    end Add_Animation_Frame;

    --  -------------------------------------------------------------------------

    procedure Add_Attack_Duration (aLine : String; theSpec : in out Spec_Data) is
        use Ada.Strings;
        Weapon_ID : Weapon_Type := Na_Wt;
        Seconds   : Float := 0.0;
        Pos1      : Natural := Fixed.Index (aLine, " ");
        Pos2      : Natural;
        Pos_P1    : constant Natural := Pos1 + 1;
    begin
        Pos2 := Fixed.Index (aLine, " ");
        Weapon_ID := Weapon_Type'Value (aLine (Pos_P1 .. Pos2 - 1));
        if Weapon_ID'Valid then
            Game_Utils.Game_Log ("ERROR: invalid weapon ID in attack event.");
            Weapon_ID := Na_Wt;
        else
            Pos1 := Fixed.Index (aLine, " ") + 1;
            Pos2 := Fixed.Index (aLine, " ");
            Seconds :=  Float'Value (aLine (Pos_P1 .. Pos2 - 1));
            theSpec.Weapon_Attack_Time (Weapon_ID) := Seconds;
        end if;
    end Add_Attack_Duration;

    --  -------------------------------------------------------------------------

    procedure Add_Attack_Event (aLine : String; theSpec : in out Spec_Data) is
        use Ada.Strings;
        Weapon_ID : Weapon_Type := Na_Wt;
        Seconds     : Float := 0.0;
        F_Val       : Float;
        Int_Val     : Integer;
        Pos1        : Natural := Fixed.Index (aLine, " ");
        Last        : constant Natural := aLine'Last;
        Pos2        : Natural;
        AEC         : Integer;
        anEvent     : Attack_Event;
    begin
        --  Skip "attack_event weapon:"
        Pos2 := Fixed.Index (Source => aLine, Pattern => " ", From => Pos1 + 1);
        Weapon_ID := Weapon_Type'Value (aLine (Pos1 .. Pos2));
        if Weapon_ID'Valid then
            --  Skip "time:"
            Pos1 := Fixed.Index (aLine, "time") + 5;
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), Seconds, Pos2);
            anEvent.Time_Sec := Single (Seconds);
            --  Skip "location:"
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), F_Val, Pos2);
            anEvent.Location (GL.X) := Single (F_Val);
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), F_Val, Pos2);
            anEvent.Location (GL.Y) := Single (F_Val);
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), F_Val, Pos2);
            anEvent.Location (GL.Z) := Single (F_Val);
            --  Skip "radius_m:"
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), F_Val, Pos2);
            anEvent.Radius := Single (F_Val);
            --  Skip "min_damage:"
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Integer_Text_IO.Get (aLine (Pos1 .. Last), Int_Val, Pos2);
            anEvent.Min_Damage := Int (Int_Val);
            --  Skip "max_damage:"
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Integer_Text_IO.Get (aLine (Pos1 .. Last), Int_Val, Pos2);
            anEvent.Max_Damage := Int (Int_Val);
            --  Skip "throw_back_mps:"
            Pos1 := Fixed.Index (aLine (Pos2 .. Last), " ");
            Ada.Float_Text_IO.Get (aLine (Pos1 .. Last), F_Val, Pos2);
            anEvent.Throw_Back_MPS := Single (F_Val);

            AEC := theSpec.Attack_Event_Count (Weapon_ID);
            if AEC < 0 or AEC >= Integer (Max_Attack_Events) then
                raise Specs_Exception with
                  "Specs_Manager.Add_Attack_Event, too many attack_event instances.";
            else
                Game_Utils.Game_Log ("Adding attack_event time: " &
                                      Single'Image (anEvent.Time_Sec));
                theSpec.Attack_Events (Weapon_ID).Append (anEvent);
                theSpec.Attack_Event_Count (Weapon_ID) :=
                  theSpec.Attack_Event_Count (Weapon_ID) + 1;
            end if;
        else
            raise Specs_Exception with
              "Specs_Manager.Add_Attack_Event invalid weapon index.";
        end if;
    end Add_Attack_Event;

    --  -------------------------------------------------------------------------

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
            Load_Specs_File (File_Name);
        end if;

        return Spec_Index;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Character_Controller.Get_Script_Index!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return -1;
    end Get_Script_Index;

    --  -------------------------------------------------------------------------


    procedure Load_Specs_File (File_Name : String) is
        use Ada.Strings;
        Path          : String := "characters/" & File_Name;
        Input_File    : File_Type;
--          Attack_Sound  : Integer := 0;
        theSpec       : Spec_Data;
    begin
        Open (Input_File, In_File, Path);
        theSpec.File_Name := To_Unbounded_String (File_Name);
        Set_Specs_Defaults (theSpec);

        while not End_Of_File (Input_File) loop
            declare
                aLine    : constant String := Get_Line (Input_File);
                S_Length : constant Natural := aLine'Length;
                Pos1     : constant Natural := Fixed.Index (aLine, " ");
--                  Pos2     : Natural;
                Pos_M1   : constant Natural := Pos1 - 1;
                Pos_P1   : constant Natural := Pos1 + 1;
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
                elsif aLine (1 .. Pos_M1) = "sprite_map_cols" then
                    theSpec.Atlas_Cols :=
                      Integer'Value (aLine (Pos_P1 .. S_Length));
                elsif aLine (1 .. Pos_M1) = "add_anim_frame" then
                    Add_Animation_Frame (aLine, theSpec);
                elsif aLine (1 .. Pos_M1) = "attack_duration" then
                    Add_Attack_Duration (aLine, theSpec);
                elsif aLine (1 .. Pos_M1) = "attack_event" then
                    Add_Attack_Event (aLine, theSpec);

                end if;
            end;  --  declare block
        end loop;

        Close (Input_File);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Specs_Manager.Load_Specs_File!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
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
