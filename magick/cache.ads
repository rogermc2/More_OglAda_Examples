
with Interfaces.C;
with Interfaces.C.Strings;

package Cache is

   type Cache_Type is (Undefined_Cache,
                       Disk_Cache,
                       Distributed_Cache,
                       Map_Cache,
                       Memory_Cache,
                       Ping_Cache);

   type Cache is new Interfaces.C.Strings.chars_ptr;

   --  AcquirePixelCache is declared in cache-private.h
--     function Acquire_Pixel_Cache (Size : size_t) return Cache;
--     pragma Import (C, Acquire_Pixel_Cache, "AcquirePixelCache");

end Cache;
