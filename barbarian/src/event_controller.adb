
with Ada.Containers.Doubly_Linked_Lists;

with Game_Utils;
with Prop_Renderer;
with Text;

package body Event_Controller is

   type Code_Received_Data is record
      Receiver_Type : RX_Type := Rx_Invalid;
      Index         : Natural := 0;
   end record;

   package Code_Received_Package is new
     Ada.Containers.Doubly_Linked_Lists (Code_Received_Data);
   type Code_Received_List is new Code_Received_Package.List with null record;

   Received_Codes : array (1 .. Max_Events) of Code_Received_List;

   procedure Check_Code_Validity (Name : String; Code : Natural);

   --  -------------------------------------------------------------------------

   procedure Add_Receiver (Code  : Natural; RX_Kind : RX_Type; Index : Natural) is
      Received_Code : Code_Received_Data;
   begin
      Check_Code_Validity ("Add_Receiver", Code);
      if RX_Kind = Rx_Invalid then
         raise Event_Controller_Exception with
           "Event_Controller.Add_Receiver, invalid RX_Kind";
      end if;

      Received_Codes (Code + 1).Append ((RX_Kind, Index));

   end Add_Receiver;

   --  -------------------------------------------------------------------------

   procedure Check_Code_Validity (Name : String; Code : Natural) is
   begin
      if Code > Max_Events then
         raise Event_Controller_Exception with
           "Event_Controller." & Name & ", invalid code: " &
           Natural'Image (Code);
      end if;
   end Check_Code_Validity;

   --  -------------------------------------------------------------------------

   procedure Reset is
   begin
      for index in 1 .. Max_Events loop
         Received_Codes (index).Clear;
      end loop;
   end Reset;

   --  -------------------------------------------------------------------------

   procedure Transmit_Code (Code : Natural) is
      use Code_Received_Package;
      Codes      : constant Code_Received_List := Received_Codes (Code + 1);
      Curs       : Cursor := Codes.First;
      Code_Data  : Code_Received_Data;
      Result     : Boolean := False;
   begin
      Check_Code_Validity ("Transmit_Code", Code);
      while Has_Element (Curs) loop
         Code_Data := Element (Curs);
         case Code_Data.Receiver_Type is
            when Rx_Story =>
               if not Text.Trigger_Comic_Text (Code_Data.Index) then
                  raise Event_Controller_Exception with
                  "Event_Controller.Transmit_Code, Trigger_Comic_Text failed.";
               end if;
            when Rx_Door =>
               Result := Prop_Renderer.Activate_Door (Code_Data.Index);
            when others => null;
         end case;
         Next (Curs);
      end loop;
   end Transmit_Code;

   --  -------------------------------------------------------------------------

end Event_Controller;
