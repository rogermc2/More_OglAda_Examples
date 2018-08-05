
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

limited with Magick_Blob.API;

--  Magic_Blob corresponds to Magic++ Blob
package Magick_Blob is
   use GL.Types;

   type Allocator is (Malloc_Allocator, New_Allocator);
   pragma Convention (C, Allocator);

   package Blob_Package is new Ada.Containers.Doubly_Linked_Lists (UByte);
   type Blob_Data is new Blob_Package.List with null record;

   function Get_Data (aBlob : Magick_Blob.API.Class_Blob.Blob) return Blob_Data;

end Magick_Blob;
