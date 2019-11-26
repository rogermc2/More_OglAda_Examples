
with Ada.Text_IO; use Ada.Text_IO;

package body Ogldev_Util is
    generic
        type Index_Type is (<>);
        type Vector_Type is  array (Index_Type) of aliased GL.Types.Single;
    procedure Print_Singles_Vector (Name : String; aVector : Vector_Type);

    procedure Print_Singles_Vector (Name : String; aVector : Vector_Type) is
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
    end Print_Singles_Vector;

    --  -------------------------------------------------------------------

    procedure Print_Singles_Vector8 is
      new Print_Singles_Vector (Maths.Index_8, Maths.Vector8);

    --  ---------------------------------------------------------------

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

    procedure Print_GL_Array8 (Name : String; anArray : Maths.Vector8_Array;
                               Start, Finish : GL.Types.Int) is
        use GL.Types;
        Last : GL.Types.Int := anArray'Length;
    begin
        if Finish < Last then
            Last := Finish;
        end if;
        Put_Line (Name & ": ");
        for Index in Start .. Last loop
            Print_Singles_Vector8 ("", anArray (Index));
        end loop;
        New_Line;
    end Print_GL_Array8;

    --  ------------------------------------------------------------------------

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
