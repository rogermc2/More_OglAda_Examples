
with Interfaces.C; use Interfaces.C;

package Method_Attribute is

   Magick_Core_Signature : constant unsigned_long := 16#abacadab#;
   pragma Convention (C, Magick_Core_Signature);
   Max_Text_Extent       : constant unsigned := 4096;

end Method_Attribute;
