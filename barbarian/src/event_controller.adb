
with Ada.Containers.Doubly_Linked_Lists;

with Game_Utils;

package body Event_Controller is

   package Code_Received_Package is new
     Ada.Containers.Doubly_Linked_Lists (Code_Received_Data);
   type Code_Received_List is new Code_Received_Package.List with null record;

   Received_Codes : Code_Received_List;

   --  -------------------------------------------------------------------------

   procedure Add_Receiver (Code  : Natural; RX_Kind : RX_Type; Index : Natural) is
      Received_Code : Code_Received_Data;
   begin
      if Code > Max_Events then
         raise Event_Controller_Exception with
           "Event_Controller.Add_Receiver, invalid code: " & Natural'Image (Code);
      end if;
      if RX_Kind = Rx_Invalid then
         raise Event_Controller_Exception with
           "Event_Controller.Add_Receiver, invalid RX_Kind";
      end if;
      Received_Codes.Append ((RX_Kind, Code, Index));

   end Add_Receiver;

   --  -------------------------------------------------------------------------

   procedure Reset is
   begin
      Received_Codes.Clear;
   end Reset;

   --  -------------------------------------------------------------------------

end Event_Controller;
