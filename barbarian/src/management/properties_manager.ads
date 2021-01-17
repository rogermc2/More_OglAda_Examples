
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Maths;

with Event_Controller;
with Mesh_Loader;

package Properties_Manager is

   type Activator_Type is (Prop_Activator_Player_State, Prop_Activator_Npc_State,
                           Prop_Activator_Prop_State);

   Properties_Exception : Exception;

   procedure Load_Properties (Prop_File : File_Type);

end Properties_Manager;
