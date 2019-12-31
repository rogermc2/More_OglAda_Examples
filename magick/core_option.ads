
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Magick_Type;

package Core_Option is

   --  From MagickCore/option
   type Option_Info is record
      Mnemonic : chars_ptr := New_String ("");  -- ../Magick++/lib/Magick++2/../../../MagickCore/option.h:166
      C_Type   : aliased Magick_Type.ssize_t := 0;  -- ../Magick++/lib/Magick++2/../../../MagickCore/option.h:169
      Flags    : aliased Magick_Type.ssize_t := 0;  -- ../Magick++/lib/Magick++2/../../../MagickCore/option.h:170
      Stealth  : aliased Magick_Type.Magick_Boolean_Type :=
        Magick_Type.Magic_False;  -- ../Magick++/lib/Magick++2/../../../MagickCore/option.h:173
   end record;
   pragma Convention (C_Pass_By_Copy, Option_Info);  -- ../Magick++/lib/Magick++2/../../../MagickCore/option.h:163

end Core_Option;
