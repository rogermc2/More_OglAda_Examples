
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

package body Ogldev_Util is

    procedure Print_Singles_Vector11 (Name : String; aVector : Ogldev_Math.Vector11) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (GL.Types.Single'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Singles_Vector11;

   --  -------------------------------------------------------------------

   procedure Print_GL_Array11 (Name : String; anArray : Ogldev_Math.Vector11_Array) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in anArray'First .. anArray'Last loop
         Print_Singles_Vector11 ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array11;

   --  ------------------------------------------------------------------------

end Ogldev_Util;
