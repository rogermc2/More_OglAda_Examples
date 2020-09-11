with Ada.Calendar;
with Ada.Streams.Stream_IO;
--  with Ada.Unchecked_Deallocation;

with GID;

package body GID_Image_Loader is
          
      procedure To_Array (Header : in out GID.Image_descriptor;
                          Width, Height, Num_Components : GL.Types.Int;
                          Data_Access : out Raw_Data_Ptr) is
         use type GL.Types.Int;          
         Cur_Pos : GL.Types.Int := 0;
         
        --  After Set_X_Y, next pixel is meant to be displayed at position (x,y)
         procedure Set_X_Y (X, Y : Natural) is
         begin
            Cur_Pos := 
              Num_Components * (GL.Types.Int (X) + GL.Types.Int (Y) * Width);
         end Set_X_Y;
         
        --  When Put_Pixel is called twice without a Set_X_Y in between,
        --  the pixel must be displayed on the next X position after the last one.
        --  Rationale: if the image lands into an array with contiguous pixels
        --  on the X axis this approach allows full address calculation to be
        --  made only at the beginning of each row which is much faster.
  
         procedure Put_Pixel (R, G, B, A : Component) is
            Input : constant array (GL.Types.Int'(0) .. GL.Types.Int'(3)) of
                  Component := (R, G, B, A);
         begin
            for I in  0 .. Num_Components - 1 loop
               Data_Access (Cur_Pos + I) := Input (I);
            end loop;
            Cur_Pos := Cur_Pos + Num_Components;
         end Put_Pixel;
         
         procedure Feedback (Percents : Natural) is null;  
         procedure Content_Loader is new GID.Load_image_contents
           (Component, Set_X_Y, Put_Pixel, Feedback, GID.nice);         
         Next_Frame : Ada.Calendar.Day_Duration;
         
      begin
         Data_Access := new Raw_Data (0 .. Width * Height * Num_Components - 1);
         Content_Loader (Header, Next_Frame);
      end To_Array;
   
   procedure Load_Image (Source : in out Ada.Streams.Root_Stream_Type'Class;
                         Pixel_Width, Pixel_Height : in out GL.Types.Int;
                         Num_Components : GL.Types.Int;
                         Data_Ptr : out Raw_Data_Ptr;
                         Data_Length : out GL.Types.Int;
                         Try_TGA : Boolean := False) is
      Header          : GID.Image_descriptor;
   begin
      GID.Load_image_header (Header, Source, Try_TGA);
      Pixel_Width :=  GL.Types.Int (GID.Pixel_width (Header));
      Pixel_Height :=  GL.Types.Int (GID.Pixel_height (Header));
      To_Array (Header, Pixel_Width, Pixel_Height, Num_Components, Data_Ptr);
      Data_Length := Data_Ptr.all'Length;
   end Load_Image;
   
   procedure Load_File_To_Image (Path : String; Data_Access : out Raw_Data_Ptr;
                                 Data_Length : out GL.Types.Int;
                                 Pixel_Width, Pixel_Height : in out GL.Types.Int;
                                 Num_Components : GL.Types.Int := 1;
                                 Try_TGA : Boolean := False) is
      use Ada.Streams.Stream_IO;      
      Input_File   : File_Type;
      Input_Stream : Stream_Access;
   begin
      Ada.Streams.Stream_IO.Open (Input_File, In_File, Path);
      Input_Stream := Stream (Input_File);
      Load_Image (Input_Stream.all, Pixel_Width, Pixel_Height, Num_Components,
                  Data_Access, Data_Length, Try_TGA);
      Ada.Streams.Stream_IO.Close (Input_File);
   end Load_File_To_Image;
   
end GID_Image_Loader;
