
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Strings.Unbounded;

with GL.Types;

with Magick_Type; use Magick_Type;
with Semaphore;

package Magick_Exception is

  type Exception_Type is (Undefined_Exception,
                          Warning_Exception,
                          Type_Warning,
                          Option_Warning,
                          Delegate_Warning,
                          Missing_Delegate_Warning,
                          Corrupt_Image_Warning,
                          File_Open_Warning,
                          Blob_Warning,
                          Stream_Warning,
                          Cache_Warning,
                          Coder_Warning,
                          Filter_Warning,
                          Module_Warning,
                          Draw_Warning,
                          Image_Warning,
                          Wand_Warning,
                          Random_Warning,
                          XServer_Warning,
                          Monitor_Warning,
                          Registry_Warning,
                          Configure_Warning,
                          Policy_Warning,
                          Error_Exception,
                          Type_Error,
                          Option_Error,
                          Delegate_Error,
                          Missing_Delegate_Error,
                          Corrupt_Image_Error,
                          File_Open_Error,
                          Blob_Error,
                          Stream_Error,
                          Cache_Error,
                          Coder_Error,
                          Filter_Error,
                          Module_Error,
                          Draw_Error,
                          Image_Error,
                          Wand_Error,
                          Random_Error,
                          XServer_Error,
                          Monitor_Error,
                          Registry_Error,
                          Configure_Error,
                          Policy_Error,
                          Fatal_Error_Exception,
                          Type_Fatal_Error,
                          Option_Fatal_Error,
                          Delegate_Fatal_Error,
                          Missing_Delegate_Fatal_Error,
                          Corrupt_Image_Fatal_Error,
                          File_Open_Fatal_Error,
                          Blob_Fatal_Error,
                          Stream_Fatal_Error,
                          Cache_Fatal_Error,
                          Coder_Fatal_Error,
                          Filter_Fatal_Error,
                          Module_Fatal_Error,
                          Draw_Fatal_Error,
                          Image_Fatal_Error,
                          Wand_Fatal_Error,
                          Random_Fatal_Error,
                          XServer_Fatal_Error,
                          Monitor_Fatal_Error,
                          Registry_Fatal_Error,
                          Configure_Fatal_Error,
                          Policy_Fatal_Error);
   pragma Convention (C, Exception_Type);

   type Exception_Info is private;

   type AI_Exception_Info is record
      Severity          : Exception_Type;
      Error_Number      : int;
      Reason            : chars_ptr;
      Error_Description : chars_ptr;
      Exceptions        : System.Address := System.Null_Address;
      Relinquish        : Magick_Boolean_Type := Magic_False;
      Sema4             : Semaphore.Sem_Ptr;
      Signature         : size_t := 0;
--          size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, AI_Exception_Info);

  function Destroy_Exception_Info (Info : access AI_Exception_Info)
      return access AI_Exception_Info;  -- ../MagickCore/exception.h:147
   pragma Import (C, Destroy_Exception_Info, "DestroyExceptionInfo");

   function Resource_Limit_Warning return Exception_Type renames Warning_Exception;
   function Resource_Limit_Error return Exception_Type renames Error_Exception;
   function Resource_Limit_Fatal_Error return Exception_Type renames
     Fatal_Error_Exception;
   function Get_Description (Info : AI_Exception_Info) return chars_ptr;
   function Get_Severity (Info : AI_Exception_Info) return Exception_Type;

private

   type Exception_Info is record
      Severity          : Exception_Type;
      Error_Number      : GL.Types.UInt;
      Reason            : Ada.Strings.Unbounded.Unbounded_String;
      Error_Description : Ada.Strings.Unbounded.Unbounded_String;
      Exceptions        : System.Address := System.Null_Address;
      Relinquish        : Boolean := False;
      Sema4             : Semaphore.Sem_Ptr;
   end record;

   for Exception_Type use (Undefined_Exception => 0,
                           Warning_Exception => 300,
                           Type_Warning => 305,
                           Option_Warning => 310,
                           Delegate_Warning => 315,
                           Missing_Delegate_Warning => 320,
                           Corrupt_Image_Warning => 325,
                           File_Open_Warning => 330,
                           Blob_Warning => 335,
                           Stream_Warning => 340,
                           Cache_Warning => 345,
                           Coder_Warning => 350,
                           Filter_Warning => 352,
                           Module_Warning => 355,
                           Draw_Warning => 360,
                           Image_Warning => 365,
                           Wand_Warning => 370,
                           Random_Warning => 375,
                           XServer_Warning => 380,
                           Monitor_Warning => 385,
                           Registry_Warning => 390,
                           Configure_Warning => 395,
                           Policy_Warning => 399,
                           Error_Exception => 400,
                           Type_Error => 405,
                           Option_Error => 410,
                           Delegate_Error => 415,
                           Missing_Delegate_Error => 420,
                           Corrupt_Image_Error => 425,
                           File_Open_Error => 430,
                           Blob_Error => 435,
                           Stream_Error => 440,
                           Cache_Error => 445,
                           Coder_Error => 450,
                           Filter_Error => 452,
                           Module_Error => 455,
                           Draw_Error => 460,
                           Image_Error => 465,
                           Wand_Error => 470,
                           Random_Error => 475,
                           XServer_Error => 480,
                           Monitor_Error => 485,
                           Registry_Error => 490,
                           Configure_Error => 495,
                           Policy_Error => 499,
                           Fatal_Error_Exception => 700,
                           Type_Fatal_Error => 705,
                           Option_Fatal_Error => 710,
                           Delegate_Fatal_Error => 715,
                           Missing_Delegate_Fatal_Error => 720,
                           Corrupt_Image_Fatal_Error => 725,
                           File_Open_Fatal_Error => 730,
                           Blob_Fatal_Error => 735,
                           Stream_Fatal_Error => 740,
                           Cache_Fatal_Error => 745,
                           Coder_Fatal_Error => 750,
                           Filter_Fatal_Error => 752,
                           Module_Fatal_Error => 755,
                           Draw_Fatal_Error => 760,
                           Image_Fatal_Error => 765,
                           Wand_Fatal_Error => 770,
                           Random_Fatal_Error => 775,
                           XServer_Fatal_Error => 780,
                           Monitor_Fatal_Error => 785,
                           Registry_Fatal_Error => 790,
                           Configure_Fatal_Error => 795,
                           Policy_Fatal_Error => 799);

end Magick_Exception;
