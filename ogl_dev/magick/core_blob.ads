
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;


with Magick_Type; use Magick_Type;
with Method_Attribute;
with Semaphore;
with Stream;

package Core_Blob is

   -- File_To_Image and Image_To_Blob moved to Magick_Image to avoid circularities

   --  blob: the address of a character stream in one of the image formats
   --        understood by ImageMagick.
   type Blob is new Interfaces.C.Strings.chars_ptr;

   type Blob_Info is private;
   type Blob_Info_Ptr is access all Blob_Info;
--     type Blob_Ref is private;
   type Allocator is (Malloc_Allocator, New_Allocator);

   Magick_Max_Buffer_Extent : constant size_t := 81920;
   Magick_Max_Blob_Extent   : constant size_t := 8 * 8192;

   type Blob_Mode is (Undefined_Blob_Mode, Read_Blob_Mode,
                      Read_Binary_Blob_Mode, Write_Blob_Mode,
                      Write_Binary_Blob_Mode, Append_Blob_Mode,
                      Append_Binary_Blob_Mode);
   pragma Convention (C, Blob_Mode);

   type Map_Mode is (Read_Mode, Write_Mode, IO_Mode,
                     Persistant_Mode);
   pragma Convention (C, Map_Mode);

   type Stream_Type is (Undefined_Stream, File_Stream, Standard_Stream,
                        Pipe_Stream, Zip_Stream, BZip_Stream,
                        Fifo_Stream, Blob_Stream, Custom_Stream);
   pragma Convention (C, Stream_Type);

--   type Blob is record
--        Data     : GL.Objects.Textures.Image_Source;
--        Length   : GL.Types.UInt := 0;
--        Allocate : Allocator;
--        Lock     : GL.Types.Size := 0;  --  Lock : Mutex_Lock;
--     end record;

   type Custom_Stream_Handler is
     access function (aBlob : access Blob; arg2 : System.Address;
                      arg3 : size_t) return size_t;
   pragma Convention (C, Custom_Stream_Handler);

   type Custom_Stream_Seeker is
     access function (aBlob : access Blob; arg2 : System.Address;
                      arg3 : size_t) return Magick_Type.Magick_Offset_Type;
   pragma Convention (C, Custom_Stream_Seeker);

   type Custom_Stream_Teller is
     access function (aBlob : access Blob; arg2 : System.Address;
                      arg3 : size_t) return Magick_Type.Magick_Offset_Type;
    pragma Convention (C, Custom_Stream_Teller);

   type Custom_Stream_Info is record
      Reader    : Custom_Stream_Handler;
      Writer    : Custom_Stream_Handler;
      Seeker    : Custom_Stream_Seeker;
      Teller    : Custom_Stream_Teller;
      Data      : System.Address;
      Signature : size_t := size_t (Method_Attribute.Magick_Core_Signature);
   end record;
    pragma Convention (C_Pass_By_Copy, Custom_Stream_Info);

   type Custom_Stream_Ptr is access all Custom_Stream_Info;

   Blob_Exception : Exception;

   --     function To_Blob (Ref : Blob_Ref) return Blob
   procedure Set_Blob_Type (Info : in out Blob_Info; theType : Stream_Type);

private
 --  From Blob.c
   type Blob_Info is record
      Length        : size_t := 0;
      Extenth       : size_t := 0;
      Quantum       : size_t := Magick_Max_Blob_Extent;
      Mode          : Blob_Mode := Undefined_Blob_Mode;
      Mapped        : Magick_Boolean_Type := Magic_False;
      EOF           : Magick_Boolean_Type := Magic_False;
      Offset        : Magick_Offset_Type := 0;
      Size          : Magick_Size_Type := 0;
      Exempt        : Magick_Boolean_Type := Magic_False;
      Synchronize   : Magick_Boolean_Type := Magic_False;
      Status        : Magick_Boolean_Type := Magic_False;
      Temporary     : Magick_Boolean_Type := Magic_False;
      Stream_Kind   : Stream_Type := Undefined_Stream;
      File_Data     : System.Address := System.Null_Address; --  File_Info;
      Properties    : System.Address := System.Null_Address; --  sys.stat.struct stat
      Blob_Stream   : Stream.Stream_Handler;
      Custom_Stream : Custom_Stream_Ptr := Null;
      Data          : access unsigned_char;
      Debug         : Magick_Boolean_Type := Magic_False;
      Sema4         : Semaphore.Sem_Ptr := Null;
      Ref_Count     : Long_Long_Integer := 1;  --  ssize_t
      Signature     : size_t :=
        size_t (Method_Attribute.Magick_Core_Signature);
   end record;
    pragma Convention (C_Pass_By_Copy, Blob_Info);

 --  From Blob.c
--     type File_Info is record
--        File : access FILE;
--     end record;
--     pragma Convention (C, File_Info);

   --  From Magic++ BlobRef.h
--     type Blob_Ref is record
--        Data     : System.Address;
--        Length   : size_t;
--        Allocate : Allocator;
--        Lock     : size_t;  --   Lock : Mutex_Lock;
--     end record;
--     pragma Convention (C_Pass_By_Copy, Blob_Ref);

--     type Blob_API is record
--        Ref : Blob_Ref;
--     end record;
--     pragma Convention (C_Pass_By_Copy, Blob);

end Core_Blob;
