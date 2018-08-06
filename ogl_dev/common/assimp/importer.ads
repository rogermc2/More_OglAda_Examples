
with GL.Types;

with Scene;

package Importer is

--      type Importer is record
--          Impl : Importer_Pimple;
--      end record;
    function Read_File (File_Name : String; Flags : GL.Types.UInt) return Scene.AI_Scene;
end Importer;
