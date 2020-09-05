
package body Shader_Manager is

    procedure Init_Fallback;

    --  -------------------------------------------------------------------------

    function Init_Shaders return Boolean is
    begin
        Init_Fallback;
        return True;
    end Init_Shaders;

    --  -------------------------------------------------------------------------

    procedure Init_Fallback is
    begin
        null;
    end Init_Fallback;

    --  -------------------------------------------------------------------------

end Shader_Manager;
