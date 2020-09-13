
package GUI is

    procedure Draw_Controller_Button_Overlays (Elapsed : Float);
    function Init_GUIs return Boolean;
    function Load_Controller_Textures return Boolean;
    procedure Load_Gui_Shaders;
    procedure Set_GUI_Gold (Amount : Integer);
    procedure Set_GUI_Javalin_Ammo (Amount : Integer);
    procedure Set_GUI_Kills (Amount : Integer);

end GUI;
