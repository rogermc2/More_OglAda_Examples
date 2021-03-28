
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

  procedure Delete_Script_Data (Script_Index : Positive);
  function Get_Property_Data (Prop_Index : Positive)
                              return Prop_Renderer_Support.Property_Data;
  function Get_Script_Data (Script_Index : Positive)
                            return Prop_Renderer_Support.Prop_Script;
  function Index_Is_Valid (Prop_Index : Positive) return Boolean;
  procedure Load_Properties (Prop_File : File_Type);
  procedure Replace_Property (Property_Index : Positive;
                              Property : Prop_Renderer_Support.Property_Data);

private
    use Prop_Renderer_Support;

    Max_Mirrors     : constant Integer := 16;
    Sprite_Y_Offset : constant Single := 0.125;

    package Properties_Package is new Ada.Containers.Vectors
      (Positive, Property_Data);
    subtype Properties_List is Properties_Package.Vector;

    package Properties_Script_Package is new Ada.Containers.Vectors
      (Positive, Prop_Script);
    subtype Properties_Script_List is Properties_Script_Package.Vector;

    Mirror_Indices            : array (1 .. Max_Mirrors) of Natural :=
                                  (others => 0);
    Mirror_Count              : Natural := 0;
    Live_Mirror_Count         : Natural := 0;
    Properties                : Properties_List;        --  g_props
    Prop_Scripts              : Properties_Script_List; --  g_prop_scripts

end Properties_Manager;
