
package Event_Controller is

  Max_Events : constant Integer := 1024;

  type RX_Type is (Rx_Invalid, Rx_Story, Rx_Door,
                   Rx_Boulder, Rx_Dart_Trap, Rx_Elevator);

   Event_Controller_Exception : Exception;

   procedure Add_Receiver (Code  : Natural; RX_Kind : RX_Type; Index : Natural);
   procedure Reset;

end Event_Controller;
