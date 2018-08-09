
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

limited with Magick_Blob.API;

--  Magic_Blob corresponds to Magic++ Blob
package Magick_Blob is
   use GL.Types;

   type Data_Array is  array (UInt range <>) of UByte;

   type Allocator is (Malloc_Allocator, New_Allocator);
   pragma Convention (C, Allocator);

   package Blob_Package is new Ada.Containers.Doubly_Linked_Lists (UByte);
   subtype Blob_Data is Blob_Package.List;

end Magick_Blob;
