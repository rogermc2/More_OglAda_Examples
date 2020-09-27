--  Program for testing Barbarian parse packages
--  Author Roger Mc Murtrie
--  Created 13 Septembert 2020

with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;

with Game_Utils;
with Manifold;
with Maps_Manager;
with Texture_Manager;

procedure Barbarian_Parse_Test is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Parse Test";
    Level_Name   : constant String := "introduction";
    Path         : constant String := "src/maps/" & Level_Name & (".map");
    theMap       : Maps_Manager.Map;
begin

    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Put_Line ("Barbarian_Parse_Test starting game log.");
    Game_Utils.Restart_Game_Log;
    Put_Line ("Barbarian_Parse_Test game log started.");
    Game_Utils.Game_Log ("Game log started");

    Manifold.Init;
--          Game_Utils.Game_Log ("Barbarian_Parse_Test Manifold initialized.");
        if Texture_Manager.Init_Texture_Manager then
            pragma Warnings (Off);
--              Game_Utils.Game_Log ("Barbarian_Parse_Test Texture_Manager initialized.");
            Maps_Manager.Load_Maps (Path, theMap);
        else
            Game_Utils.Game_Log ("Barbarian_Parse_Test Init_Texture_Manager failed.");
        end if;

    Game_Utils.Close_Game_Log;
    Glfw.Shutdown;

end Barbarian_Parse_Test;

