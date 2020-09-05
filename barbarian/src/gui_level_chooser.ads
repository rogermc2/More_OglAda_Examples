
package GUI_Level_Chooser is

    procedure Init;
    function Get_Selected_Map_Name (Custom : Boolean) return String;
    function Start_Level_Chooser_Loop (Custom : Boolean) return Boolean;

end GUI_Level_Chooser;
