

package body Magick_Exception is

   function Get_Description (Info : AI_Exception_Info) return chars_ptr is
   begin
      return Info.Error_Description;
   end Get_Description;

   --  -------------------------------------------------------------------------

   function Get_Severity (Info : AI_Exception_Info) return Exception_Type is
   begin
      return Info.Severity;
   end Get_Severity;

end Magick_Exception;
