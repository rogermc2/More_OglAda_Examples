
package body Character_Controller is

   procedure Init is
   begin
        null;
   end Init;

   --  -------------------------------------------------------------------------

   --  read characters from an already open file stream
    procedure Load_Characters (Input_Stream : Stream_IO.Stream_Access;
                               Editor_Mode : Boolean) is
        use Ada.Streams;
        Input_File       : Stream_IO.File_Type;

        aLine            : Unbounded_String;
        Num_Story_Lines  : Natural;
        Story_Lines      : Story_Lines_List;
        --          MN_Start_Time : Float;  --  Debug variable
    begin
        Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
        Input_Stream := Stream_IO.Stream (Input_File);
        Unbounded_String'Read (Input_Stream, theMap.Level_Title);
        Unbounded_String'Read (Input_Stream, theMap.Level_Par_Time);

        --  Story
        Unbounded_String'Read (Input_Stream, aLine);
        declare
            aString  : constant String := To_String (aLine);
            Num_Part : constant String := aString (13 .. aString'Length);
        begin
            Num_Story_Lines := Integer'Value (Num_Part);
            for line_num in 1 .. Num_Story_Lines loop
                Unbounded_String'Read (Input_Stream, aLine);
                Story_Lines.Append (aLine);
            end loop;
        end;  --  declare block

        Unbounded_String'Read (Input_Stream, theMap.Music_Track);
        Unbounded_String'Read (Input_Stream, theMap.Hammer_Music_Track);

        Manifold.Load_Tiles (Input_Stream);

        --          MN_Start_Time := Float (Glfw.Time);  --  Set debug variable

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Properties_Manager.Load_Characters!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Characters;

    --  ----------------------------------------------------------------------------

   function Update_Characters (Seconds : Float) return Boolean is
   begin
        return False;
   end Update_Characters;

   --  -------------------------------------------------------------------------

end Character_Controller;
