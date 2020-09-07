
with GL.Types;

package MMenu is

    function Are_We_In_Custom_Maps return Boolean;
    function Did_User_Choose_Custom_Maps return Boolean;
    function Did_User_Choose_New_Game return Boolean;
    procedure Draw_Menu (Elapsed : Float);
    procedure Draw_Title_Only;
    function End_Story_Open return Boolean;
    procedure Init_MMenu;
    procedure Start_Mmenu_Title_Bounce;
    function Update_MMenu (Delta_Time : Float) return Boolean;

end MMenu;
