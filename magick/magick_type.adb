
package body Magick_Type is


   function To_Boolean (Bool : Magick_Boolean_Type) return Boolean is
   begin
      return Bool = Magic_True;
   end To_Boolean;

   --  -------------------------------------------------------------------------

   function To_Magick_Boolean (Bool : Boolean) return Magick_Boolean_Type is
   begin
      if Bool then
         return Magic_True;
      else
         return Magic_False;
      end if;

   end To_Magick_Boolean;

end Magick_Type;
