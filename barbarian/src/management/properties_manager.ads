
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Maths;

with Event_Controller;
with Mesh_Loader;
with Prop_Renderer_Support;

package Properties_Manager is

   type Activator_Type is (Prop_Activator_Player_State, Prop_Activator_Npc_State,
                           Prop_Activator_Prop_State);

   Properties_Exception : Exception;

   function Index_Is_Valid (Prop_Index : GL.Types.Int) return Boolean;
   procedure Load_Properties (Prop_File : File_Type);

private
    use Prop_Renderer_Support;

    Max_Mirrors     : constant Integer := 16;
    Sprite_Y_Offset : constant Single := 0.125;

    package Properties_Package is new Ada.Containers.Vectors
      (Positive, Property_Data);
    subtype Properties_List is Properties_Package.Vector;

    package Properties_Script_Package is new Ada.Containers.Vectors
      (Positive, Prop_Script);
    type Properties_Script_List is new Properties_Script_Package.Vector with null record;

    Portal_Index              : Natural := 0;
    Mirror_Indices            : array (1 .. Max_Mirrors) of Natural :=
                                  (others => 0);
    Mirror_Count              : Natural := 0;
    Live_Mirror_Count         : Natural := 0;
    Properties                : Properties_List;
    Prop_Scripts              : Properties_Script_List;

end Properties_Manager;
