
with GL.Types; use GL.Types;
with GL.Types.Colors;

package Text is

   Text_Exception : Exception;
   function Add_Text (theText                          : String;
                      X, Y, Size_In_Pixels, R, G, B, A : Single) return Integer;
   procedure Centre_Text (ID : Positive; X, Y : Single);
   procedure Change_Text_Colour (ID : Positive; R, G, B, A : Single);
   function Create_Text_Box (Text                    : String; Font_ID  : Integer;
                             X_Min, Y_Min, Scale     : Single;
                             Text_Colour, Box_Colour : GL.Types.Colors.Color)
                             return Integer;
   procedure Init_Comic_Texts;
   procedure Init_Particle_Texts;
   procedure Init_Text_Rendering
     (Font_image_File, Font_Metadata_File : String;
      Viewport_Width, Viewport_Height     : GL.Types.Int);
   function Is_Text_ID_Valid (ID          : Positive; File_Name : String;
                              Line_Number : Integer) return Boolean;
   procedure Move_Text (ID : Positive; X, Y : Single);
   procedure Set_Text_Visible (ID : Positive; Visible : Boolean);
   procedure Update_Text (ID : Positive; aString : String);

end Text;
