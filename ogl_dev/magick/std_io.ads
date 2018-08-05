
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Std_IO is

   subtype fpos_t is Long_Long_Integer;  -- /usr/include/_stdio.h:81

   type S_Buffer is record
      base : access unsigned_char;  -- /usr/include/_stdio.h:93
      size : aliased int;  -- /usr/include/_stdio.h:94
   end record;
   pragma Convention (C_Pass_By_Copy, S_Buffer);  -- /usr/include/_stdio.h:92

   type FILE_Buf_Array is array (0 .. 2) of aliased unsigned_char;
   type FILE_NBuf_Array is array (0 .. 0) of aliased unsigned_char;
   type FILE is record
      P        : access unsigned_char;  -- /usr/include/_stdio.h:127
      R        : aliased int;  -- /usr/include/_stdio.h:128
      W        : aliased int;  -- /usr/include/_stdio.h:129
      Flags    : aliased short;  -- /usr/include/_stdio.h:130
      File     : aliased short;  -- /usr/include/_stdio.h:131
      BF       : aliased S_Buffer;  -- /usr/include/_stdio.h:132
      Lbf_Size : aliased int;  -- /usr/include/_stdio.h:133
      Cookie   : System.Address;  -- /usr/include/_stdio.h:136
      Close    : access function (arg1 : System.Address) return int;  -- /usr/include/_stdio.h:137
      Read     : access function (arg1 : System.Address;
                                  arg2 : Interfaces.C.Strings.chars_ptr;
                                  arg3 : int) return int;  -- /usr/include/_stdio.h:138
      Seek     : access function (arg1 : System.Address;
                                  arg2 : fpos_t;
                                  arg3 : int) return fpos_t;  -- /usr/include/_stdio.h:139
      Write    : access function (arg1 : System.Address;
                                  arg2 : Interfaces.C.Strings.chars_ptr;
                                  arg3 : int) return int;  -- /usr/include/_stdio.h:140
      B        : aliased S_Buffer;  -- /usr/include/_stdio.h:143
      Extra    : System.Address;  -- /usr/include/_stdio.h:144
      uR       : aliased int;  -- /usr/include/_stdio.h:145
      Buffer   : aliased FILE_Buf_Array;  -- /usr/include/_stdio.h:148
      NBuf     : aliased FILE_NBuf_Array;  -- /usr/include/_stdio.h:149
      Lb       : aliased S_Buffer;  -- /usr/include/_stdio.h:152
      Blksize  : aliased int;  -- /usr/include/_stdio.h:155
      Offset   : aliased fpos_t;  -- /usr/include/_stdio.h:156
   end record;
   pragma Convention (C_Pass_By_Copy, FILE);

--     subtype FILE is S_FILE;  -- /usr/include/_stdio.h:157

end Std_IO;
