
with GL.Objects.Programs;
with Input_Callback;

package GUI_Level_Chooser is

   GUI_Level_Chooser_Exception : Exception;

   function Cheated_On_Map return Boolean;
   procedure Init;
   function Get_Hammer_Kills return Integer;
   function Get_Selected_Map_Music return String;
   function Get_Selected_Map_Name (Custom : Boolean) return String;
   procedure Increment_Hammer_Kills;
   function Is_Level_Introduction return Boolean;
   function Is_Level_Warlock return Boolean;
   function Level_Is_Unmodified return Boolean;
   procedure Set_Boulder_Crushes (Value : Integer);
   procedure Set_Cheated_On_Map (State : Boolean);
   procedure Set_Fall_Kills (Value : Integer);
   procedure Set_Hammer_Kills (Value : Integer);
   procedure Set_Pillar_Crushes (Value : Integer);
   function Start_Level_Chooser_Loop
     (Window : in out Input_Callback.Barbarian_Window;
      Credits_Shader_Program : GL.Objects.Programs.Program;
      Custom_Maps : Boolean) return Boolean;

end GUI_Level_Chooser;
