
with GL.Types;

package Text is

    function Add_Text (theText : String;
                       X, Y, Size_In_Pixels, R, G, B, A : Float) return Integer;
    procedure Centre_Text (ID : Positive; X, Y : Float);
    procedure Change_Text_Colour (ID : Positive; R, G, B, A : Float);
    procedure Init_Comic_Texts;
    procedure Init_Particle_Texts;
    procedure Init_Text_Rendering
      (Font_image_File, Font_Metadata_File : String;
        Viewport_Width, Viewport_Height : GL.Types.Int);
    function Is_Text_ID_Valid (ID : Positive; File_Name : String;
                              Line_Number : Integer) return Boolean;
    procedure Move_Text (ID : Positive; X, Y : Float);
    procedure Set_Text_Visible (ID : Positive; Visible : Boolean);
    procedure Update_Text (ID : Positive; aString : String);

end Text;
