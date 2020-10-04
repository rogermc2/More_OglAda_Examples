
package Event_Controller is

  Max_Events : constant Integer := 1024;

  type RX_Type is (Rx_Invalid, Rx_Story, Rx_Door,
                   Rx_Boulder, Rx_Dart_Trap, Rx_Elevator);

   Event_Controller_Exception : Exception;

   procedure Add_Receiver (Code  : Natural; RX_Kind : RX_Type; Index : Natural);
   procedure Reset;

private
   type Code_Received_Data is record
      Receiver_Type : RX_Type := Rx_Invalid;
      Code          : Natural := 0;
      Index         : Natural := 0;
   end record;

end Event_Controller;
