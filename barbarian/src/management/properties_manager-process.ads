
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types; use GL.Types;

with Maths;

with Event_Controller;
with Mesh_Loader;

package Properties_Manager.Process is

    --     type Prop_Type is (Generic_Prop, Boulder, Door, Dart_Trap, Touch_Plate,
    --                        Treasure, Portal, Bridge, Pillar, Decap_Head, Box,
    --                        Mirror, Tavern, Jav_Stand, Elevator, Hammer, Anim_Loop,
    --                        Food, Windlass, Big_Box, End_Camera, Pot);
    --     type Door_State is (Open, Closed, Opening, Closing);
    --     type Elevator_State is (At_Top, At_Bottom, Going_Down, Going_Up,
    --                             Waiting_To_Go_Up, Waiting_To_Go_Down);
    --
    --     type Trap_State is (Trap_Primed, Trap_Reloading);
    --
    --     type Activator_Type is (Prop_Activator_Player, Prop_Activator_Npc,
    --                             Prop_Activator_Prop);

    Properties_Process_Exception : Exception;

    function Get_Index_Of_Prop_Script (Script_File : String) return Positive;
    function Load_Property_Script (File_Name : String; Index : out Positive)
                                   return Boolean;
    procedure Process_Script_Type (New_Props : in out Prop; aScript : Prop_Script;
                                   Rx_Kind   : in out Event_Controller.RX_Type;
                                   Rebalance  : in out Boolean);

end Properties_Manager.Process;
