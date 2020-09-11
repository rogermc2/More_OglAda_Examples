
with Ada.Streams;
with Ada.Unchecked_Deallocation;

with GL.Types;

package GID_Image_Loader is

    type Component is new GL.Types.UByte;
    type Raw_Data is array (GL.Types.Int range <>) of Component;
    type Raw_Data_Ptr is access Raw_Data;
    
    procedure Free_Data is new 
      Ada.Unchecked_Deallocation (Raw_Data, Raw_Data_Ptr);
 
    --  TGA images do not have a signature so 
    --  if the source contains TGA data set Try_TGA to True.
    procedure Load_Image (Source : in out Ada.Streams.Root_Stream_Type'Class;
                          Pixel_Width, Pixel_Height : in out GL.Types.Int;
                          Num_Components : GL.Types.Int;
                          Data_Ptr : out Raw_Data_Ptr;
                          Data_Length : out GL.Types.Int;
                          Try_TGA : Boolean := False);
     
    -- Like Load_Image_To_Texture but takes the path to a file as input.
    procedure Load_File_To_Image (Path : String; Data_Access : out Raw_Data_Ptr;
                                  Data_Length : out GL.Types.Int;
                                  Pixel_Width, Pixel_Height : in out GL.Types.Int;
                                  Num_Components : GL.Types.Int := 1;
                                  Try_TGA : Boolean := False);
   
end GID_Image_Loader;
