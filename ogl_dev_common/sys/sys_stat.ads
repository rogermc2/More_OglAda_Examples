
with Interfaces.C; use Interfaces.C;
--  with i386_utypes;
--  with sys_utypes_uino_t;
--  with sys_utypes_umode_t;
--  with sys_utypes_unlink_t;
--  with sys_utypes_utimespec;
--  with sys_utypes_udev_t;
--  with sys_utypes;
--  with sys_utypes_uuid_t;
--  with sys_utypes_ugid_t;
--  with sys_utypes_uoff_t;
--  with sys_utypes_ublkcnt_t;
--  with sys_utypes_ublksize_t;
with Interfaces.C.Strings;

--  with sys_utypes_ufilesec_t;

package Sys_Stat is

   type stat_st_qspare_array is array (0 .. 1) of aliased int64;
   type stat is record
      st_dev   : aliased sys_utypes_udev_t.dev_t;  -- /usr/include/sys/stat.h:182
      st_mode  : aliased sys_utypes_umode_t.mode_t;  -- /usr/include/sys/stat.h:182
      st_nlink : aliased sys_utypes_unlink_t.nlink_t;  -- /usr/include/sys/stat.h:182
      st_ino   : aliased sys_utypes_h.uu_darwin_ino64_t;  -- /usr/include/sys/stat.h:182
      st_uid   : aliased sys_utypes_uuid_t.uid_t;  -- /usr/include/sys/stat.h:182
      st_gid   : aliased sys_utypes_ugid_t.gid_t;  -- /usr/include/sys/stat.h:182
      st_rdev  : aliased sys_utypes_udev_t.dev_t;  -- /usr/include/sys/stat.h:182
      st_atimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:182
      st_mtimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:182
      st_ctimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:182
      st_birthtimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:182
      st_size    : aliased sys_utypes_uoff_t.off_t;  -- /usr/include/sys/stat.h:182
      st_blocks  : aliased sys_utypes_ublkcnt_t.blkcnt_t;  -- /usr/include/sys/stat.h:182
      st_blksize : aliased sys_utypes_ublksize_t.blksize_t;  -- /usr/include/sys/stat.h:182
      st_flags   : aliased uint32;  -- /usr/include/sys/stat.h:182
      st_gen     : aliased uint32;  -- /usr/include/sys/stat.h:182
      st_lspare  : aliased int32;  -- /usr/include/sys/stat.h:182
      st_qspare  : aliased stat_st_qspare_array;  -- /usr/include/sys/stat.h:182
   end record;
   pragma Convention (C_Pass_By_Copy, stat);  -- /usr/include/sys/stat.h:182

   type stat64_st_qspare_array is array (0 .. 1) of aliased i386_utypes_h.uu_int64_t;
   type stat64 is record
      st_dev   : aliased sys_utypes_udev_t.dev_t;  -- /usr/include/sys/stat.h:221
      st_mode  : aliased sys_utypes_umode_t.mode_t;  -- /usr/include/sys/stat.h:221
      st_nlink : aliased sys_utypes_unlink_t.nlink_t;  -- /usr/include/sys/stat.h:221
      st_ino : aliased sys_utypes_h.uu_darwin_ino64_t;  -- /usr/include/sys/stat.h:221
      st_uid : aliased sys_utypes_uuid_t.uid_t;  -- /usr/include/sys/stat.h:221
      st_gid : aliased sys_utypes_ugid_t.gid_t;  -- /usr/include/sys/stat.h:221
      st_rdev : aliased sys_utypes_udev_t.dev_t;  -- /usr/include/sys/stat.h:221
      st_atimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:221
      st_mtimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:221
      st_ctimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:221
      st_birthtimespec : aliased sys_utypes_utimespec_h.timespec;  -- /usr/include/sys/stat.h:221
      st_size : aliased sys_utypes_uoff_t.off_t;  -- /usr/include/sys/stat.h:221
      st_blocks : aliased sys_utypes_ublkcnt_t.blkcnt_t;  -- /usr/include/sys/stat.h:221
      st_blksize : aliased sys_utypes_ublksize_t.blksize_t;  -- /usr/include/sys/stat.h:221
      st_flags : aliased i386_utypes_h.uu_uint32_t;  -- /usr/include/sys/stat.h:221
      st_gen : aliased i386_utypes_h.uu_uint32_t;  -- /usr/include/sys/stat.h:221
      st_lspare : aliased i386_utypes_h.uu_int32_t;  -- /usr/include/sys/stat.h:221
      st_qspare : aliased stat64_st_qspare_array;  -- /usr/include/sys/stat.h:221
   end record;
   pragma Convention (C_Pass_By_Copy, stat64);  -- /usr/include/sys/stat.h:221

   function chmod (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:355
   pragma Import (C, chmod, "_chmod");

   function fchmod (arg1 : int; arg2 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:356
   pragma Import (C, fchmod, "_fchmod");

   function fstat (arg1 : int; arg2 : access stat) return int;  -- /usr/include/sys/stat.h:357
   pragma Import (C, fstat, "_fstat$INODE64");

   function lstat (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : access stat) return int;  -- /usr/include/sys/stat.h:358
   pragma Import (C, lstat, "_lstat$INODE64");

   function mkdir (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:359
   pragma Import (C, mkdir, "mkdir");

   function mkfifo (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:360
   pragma Import (C, mkfifo, "mkfifo");

   function stat (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : access stat) return int;  -- /usr/include/sys/stat.h:361
   pragma Import (C, stat, "_stat$INODE64");

   function umask (arg1 : sys_utypes_umode_t.mode_t) return sys_utypes_umode_t.mode_t;  -- /usr/include/sys/stat.h:363
   pragma Import (C, umask, "umask");

   function fchmodat
     (arg1 : int;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : sys_utypes_umode_t.mode_t;
      arg4 : int) return int;  -- /usr/include/sys/stat.h:366
   pragma Import (C, fchmodat, "fchmodat");

   function fstatat
     (arg1 : int;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : access stat;
      arg4 : int) return int;  -- /usr/include/sys/stat.h:367
   pragma Import (C, fstatat, "_fstatat$INODE64");

   function mkdirat
     (arg1 : int;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:368
   pragma Import (C, mkdirat, "mkdirat");

   function futimens (uu_fd : int; uu_times : access constant sys_utypes_utimespec_h.timespec) return int;  -- /usr/include/sys/stat.h:373
   pragma Import (C, futimens, "futimens");

   function utimensat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_times : access constant sys_utypes_utimespec_h.timespec;
      uu_flag : int) return int;  -- /usr/include/sys/stat.h:374
   pragma Import (C, utimensat, "utimensat");

   function chflags (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : i386_utypes_h.uu_uint32_t) return int;  -- /usr/include/sys/stat.h:382
   pragma Import (C, chflags, "chflags");

   function chmodx_np (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:383
   pragma Import (C, chmodx_np, "chmodx_np");

   function fchflags (arg1 : int; arg2 : i386_utypes_h.uu_uint32_t) return int;  -- /usr/include/sys/stat.h:384
   pragma Import (C, fchflags, "fchflags");

   function fchmodx_np (arg1 : int; arg2 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:385
   pragma Import (C, fchmodx_np, "fchmodx_np");

   function fstatx_np
     (arg1 : int;
      arg2 : access stat;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:386
   pragma Import (C, fstatx_np, "_fstatx_np$INODE64");

   function lchflags (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : i386_utypes_h.uu_uint32_t) return int;  -- /usr/include/sys/stat.h:387
   pragma Import (C, lchflags, "lchflags");

   function lchmod (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_umode_t.mode_t) return int;  -- /usr/include/sys/stat.h:388
   pragma Import (C, lchmod, "lchmod");

   function lstatx_np
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : access stat;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:389
   pragma Import (C, lstatx_np, "_lstatx_np$INODE64");

   function mkdirx_np (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:390
   pragma Import (C, mkdirx_np, "mkdirx_np");

   function mkfifox_np (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:391
   pragma Import (C, mkfifox_np, "mkfifox_np");

   function statx_np
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : access stat;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:392
   pragma Import (C, statx_np, "_statx_np$INODE64");

   function umaskx_np (arg1 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:393
   pragma Import (C, umaskx_np, "umaskx_np");

   function fstatx64_np
     (arg1 : int;
      arg2 : access stat64;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:397
   pragma Import (C, fstatx64_np, "fstatx64_np");

   function lstatx64_np
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : access stat64;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:398
   pragma Import (C, lstatx64_np, "lstatx64_np");

   function statx64_np
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : access stat64;
      arg3 : sys_utypes_ufilesec_t.filesec_t) return int;  -- /usr/include/sys/stat.h:399
   pragma Import (C, statx64_np, "statx64_np");

   function fstat64 (arg1 : int; arg2 : access stat64) return int;  -- /usr/include/sys/stat.h:400
   pragma Import (C, fstat64, "fstat64");

   function lstat64 (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : access stat64) return int;  -- /usr/include/sys/stat.h:401
   pragma Import (C, lstat64, "lstat64");

   function stat64 (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : access stat64) return int;  -- /usr/include/sys/stat.h:402
   pragma Import (C, stat64, "stat64");

end Sys_Stat;
