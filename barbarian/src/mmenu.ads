
package MMenu is

    function Are_We_In_Custom_Maps return Boolean;
    function Did_User_Choose_Custom_Maps return Boolean;
    function Did_User_Choose_New_Game return Boolean;
    procedure Draw_Menu (Elapsed : Float);
    function End_Story_Open return Boolean;
    function Init_MMenu return Boolean;
    procedure Start_Mmenu_Title_Bounce;
    function Update_MMenu (Delta_Time : Float) return Boolean;

end MMenu;
