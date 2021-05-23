
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GL.Types; use GL.Types;

package Fmod_Common is

    type Fmod_Init_Flags is (Fmod_Init_Normal,
                             Fmod_Init_Stream_From_Update,
                             Fmod_Init_Mix_From_Update,
                             Fmod_Init_3D_Righthanded,
                             Fmod_Init_Channel_Lowpass,
                             Fmod_Init_Channel_Distancefilter,
                             Fmod_Init_Profile_Enable,
                             Fmod_Init_Vol0_Becomes_Virtual,
                             Fmod_Init_Geometry_Useclosest,
                             Fmod_Init_Prefer_Dolby_Downmix,
                             Fmod_Init_Thread_Unsafe,
                             Fmod_Init_Profile_Meter_All,
                             Fmod_Init_Memory_Tracking);
    pragma Convention (C, Fmod_Init_Flags);

    type Fmod_Mode is (Fmod_Default,
                       Fmod_Loop_Off,
                       Fmod_Loop_Normal,
                       Fmod_Loop_Bidi,
                       Fmod_2D,
                       Fmod_3D,
                       Fmod_Createstream,
                       Fmod_Createsample,
                       Fmod_Createcompressedsample,
                       Fmod_Openuser,
                       Fmod_Openmemory,
                       Fmod_Openraw,
                       Fmod_Openonly,
                       Fmod_Accuratetime,
                       Fmod_Mpegsearch,
                       Fmod_Nonblocking,
                       Fmod_Unique,
                       Fmod_3D_Headrelative,
                       Fmod_3D_Worldrelative,
                       Fmod_3D_Inverserolloff,
                       Fmod_3D_Linearrolloff,
                       Fmod_3D_Linearsquarerolloff,
                       Fmod_3D_Inversetaperedrolloff,
                       Fmod_Ignoretags,
                       Fmod_3D_Customrolloff,
                       Fmod_Lowmem,
                       Fmod_Openmemory_Point,
                       Fmod_3D_Ignoregeometry,
                       Fmod_Virtual_Playfromstart);
    pragma Convention (C, Fmod_Mode);

    type Fmod_Sound_Format is (Fmod_Sound_Format_None,
                               Fmod_Sound_Format_Pcm8,
                               Fmod_Sound_Format_Pcm16,
                               Fmod_Sound_Format_Pcm24,
                               Fmod_Sound_Format_Pcm32,
                               Fmod_Sound_Format_Pcmfloat,
                               Fmod_Sound_Format_Bitstream,
                               Fmod_Sound_Format_Max,
                               Fmod_Sound_Format_Forceint);
    pragma Convention (C, Fmod_Sound_Format);

    type Fmod_Result is (Fmod_Ok,
                         Fmod_Err_Badcommand,
                         Fmod_Err_Channel_Alloc,
                         Fmod_Err_Channel_Stolen,
                         Fmod_Err_Dma,
                         Fmod_Err_Dsp_Connection,
                         Fmod_Err_Dsp_Dontprocess,
                         Fmod_Err_Dsp_Format,
                         Fmod_Err_Dsp_Inuse,
                         Fmod_Err_Dsp_Notfound,
                         Fmod_Err_Dsp_Reserved,
                         Fmod_Err_Dsp_Silence,
                         Fmod_Err_Dsp_Type,
                         Fmod_Err_File_Bad,
                         Fmod_Err_File_Couldnotseek,
                         Fmod_Err_File_Diskejected,
                         Fmod_Err_File_Eof,
                         Fmod_Err_File_Endofdata,
                         Fmod_Err_File_Notfound,
                         Fmod_Err_Format,
                         Fmod_Err_Header_Mismatch,
                         Fmod_Err_Http,
                         Fmod_Err_Http_Access,
                         Fmod_Err_Http_Proxy_Auth,
                         Fmod_Err_Http_Server_Error,
                         Fmod_Err_Http_Timeout,
                         Fmod_Err_Initialization,
                         Fmod_Err_Initialized,
                         Fmod_Err_Internal,
                         Fmod_Err_Invalid_Float,
                         Fmod_Err_Invalid_Handle,
                         Fmod_Err_Invalid_Param,
                         Fmod_Err_Invalid_Position,
                         Fmod_Err_Invalid_Speaker,
                         Fmod_Err_Invalid_Syncpoint,
                         Fmod_Err_Invalid_Thread,
                         Fmod_Err_Invalid_Vector,
                         Fmod_Err_Maxaudible,
                         Fmod_Err_Memory,
                         Fmod_Err_Memory_Cantpoint,
                         Fmod_Err_Needs3D,
                         Fmod_Err_Needshardware,
                         Fmod_Err_Net_Connect,
                         Fmod_Err_Net_Socket_Error,
                         Fmod_Err_Net_Url,
                         Fmod_Err_Net_Would_Block,
                         Fmod_Err_Notready,
                         Fmod_Err_Output_Allocated,
                         Fmod_Err_Output_Createbuffer,
                         Fmod_Err_Output_Drivercall,
                         Fmod_Err_Output_Format,
                         Fmod_Err_Output_Init,
                         Fmod_Err_Output_Nodrivers,
                         Fmod_Err_Plugin,
                         Fmod_Err_Plugin_Missing,
                         Fmod_Err_Plugin_Resource,
                         Fmod_Err_Plugin_Version,
                         Fmod_Err_Record,
                         Fmod_Err_Reverb_Channelgroup,
                         Fmod_Err_Reverb_Instance,
                         Fmod_Err_Subsounds,
                         Fmod_Err_Subsound_Allocated,
                         Fmod_Err_Subsound_Cantmove,
                         Fmod_Err_Tagnotfound,
                         Fmod_Err_Toomanychannels,
                         Fmod_Err_Truncated,
                         Fmod_Err_Unimplemented,
                         Fmod_Err_Uninitialized,
                         Fmod_Err_Unsupported,
                         Fmod_Err_Version,
                         Fmod_Err_Event_Already_Loaded,
                         Fmod_Err_Event_Liveupdate_Busy,
                         Fmod_Err_Event_Liveupdate_Mismatch,
                         Fmod_Err_Event_Liveupdate_Timeout,
                         Fmod_Err_Event_Notfound,
                         Fmod_Err_Studio_Uninitialized,
                         Fmod_Err_Studio_Not_Loaded,
                         Fmod_Err_Invalid_String,
                         Fmod_Err_Already_Locked,
                         Fmod_Err_Not_Locked,
                         Fmod_Err_Record_Disconnected,
                         Fmod_Err_Toomanysamples,
                         Fmod_Result_Forceint);
    pragma Convention (C, Fmod_Result);

    type Fmod_Create_Sound_EXINFO is record
        Cbsize      : Int;
        Length : UInt;
        Fileoffset : UInt;
        Numchannels : Int;
        Defaultfrequency : Int;
        Format : Fmod_Sound_Format;
        Decodebuffersize : UInt;
        Initialsubsound : Int;
        Numsubsounds : Int;
        Inclusionlist : access Int;
        Inclusionlistnum : Int;
        Pcmreadcallback : Fmod_Sound_Pcmread_Callback;
        Pcmsetposcallback : Fmod_Sound_Pcmsetpos_Callback;
        Nonblockcallback : Fmod_Sound_Nonblock_Callback;
        Dlsname : chars_ptr;
        Encryptionkey : chars_ptr;
        Maxpolyphony : Int;
        Userdata : GLvoid_Ptr;
        Suggestedsoundtype :  Fmod_Sound_Type;
        Fileuseropen :  Fmod_File_Open_Callback;
        Fileuserclose : Fmod_File_Close_Callback;
        Fileuserread : Fmod_File_Read_Callback;
        Fileuserseek : Fmod_File_Seek_Callback;
        Fileuserasyncread : Fmod_File_Asyncread_Callback;
        Fileuserasynccancel : Fmod_File_Asynccancel_Callback;
        Fileuserdata : GLvoid_Ptr;
        Filebuffersize : Int;
        Channelorder : Fmod_Channelorder;
        Initialsoundgroup : access Fmod_Soundgroup;
        Initialseekposition : Int;
        Initialseekpostype : Fmod_Timeunit;
        Ignoresetfilesystem : Int;
        Audioqueuepolicy : UInt;
        Minmidigranularity : UInt;
        Nonblockthreadid : Int;
        Fsbguid : access Fmod_Guid;
    end record;

private

    for Fmod_Init_Flags use (Fmod_Init_Normal                => 16#00000000#,
                             Fmod_Init_Stream_From_Update    => 16#00000001#,
                             Fmod_Init_Mix_From_Update       => 16#00000002#,
                             Fmod_Init_3D_Righthanded        => 16#00000004#,
                             Fmod_Init_Channel_Lowpass       => 16#00000100#,
                             Fmod_Init_Channel_Distancefilter => 16#00000200#,
                             Fmod_Init_Profile_Enable        => 16#00010000#,
                             Fmod_Init_Vol0_Becomes_Virtual  => 16#00020000#,
                             Fmod_Init_Geometry_Useclosest   => 16#00040000#,
                             Fmod_Init_Prefer_Dolby_Downmix  => 16#00080000#,
                             Fmod_Init_Thread_Unsafe         => 16#00100000#,
                             Fmod_Init_Profile_Meter_All     => 16#00200000#,
                             Fmod_Init_Memory_Tracking       => 16#00400000#);

    for Fmod_Mode use (Fmod_Default => 16#00000000#,
                       Fmod_Loop_Off                 => 16#00000001#,
                       Fmod_Loop_Normal              => 16#00000002#,
                       Fmod_Loop_Bidi                => 16#00000004#,
                       Fmod_2D                       => 16#00000008#,
                       Fmod_3D                       => 16#00000010#,
                       Fmod_Createstream             => 16#00000080#,
                       Fmod_Createsample             => 16#00000100#,
                       Fmod_Createcompressedsample   => 16#00000200#,
                       Fmod_Openuser                 => 16#00000400#,
                       Fmod_Openmemory               => 16#00000800#,                       Fmod_Openraw                  => 16#00001000#,
                       Fmod_Openonly                 => 16#00002000#,
                       Fmod_Accuratetime             => 16#00004000#,
                       Fmod_Mpegsearch               => 16#00008000#,
                       Fmod_Nonblocking              => 16#00010000#,
                       Fmod_Unique                   => 16#00020000#,
                       Fmod_3D_Headrelative          => 16#00040000#,
                       Fmod_3D_Worldrelative         => 16#00080000#,
                       Fmod_3D_Inverserolloff        => 16#00100000#,
                       Fmod_3D_Linearrolloff         => 16#00200000#,
                       Fmod_3D_Linearsquarerolloff   => 16#00400000#,
                       Fmod_3D_Inversetaperedrolloff => 16#00800000#,
                       Fmod_Ignoretags               => 16#02000000#,
                       Fmod_3D_Customrolloff         => 16#04000000#,
                       Fmod_Lowmem                   => 16#08000000#,
                       Fmod_Openmemory_Point         => 16#10000000#,
                       Fmod_3D_Ignoregeometry        => 16#40000000#,
                       Fmod_Virtual_Playfromstart    => 16#80000000#);

    for Fmod_Sound_Format use (Fmod_Sound_Format_None       => 16#000001#,
                               Fmod_Sound_Format_Pcm8       => 16#000002#,
                               Fmod_Sound_Format_Pcm16      => 16#000003#,
                               Fmod_Sound_Format_Pcm24      => 16#000004#,
                               Fmod_Sound_Format_Pcm32      => 16#000005#,
                               Fmod_Sound_Format_Pcmfloat   => 16#000006#,
                               Fmod_Sound_Format_Bitstream  => 16#000007#,
                               Fmod_Sound_Format_Max        => 16#000008#,
                               Fmod_Sound_Format_Forceint   => 16#065536#);

    for Fmod_Result use (Fmod_Ok => 16#000000#,
                         Fmod_Err_Badcommand => 16#000001#,
                         Fmod_Err_Channel_Alloc => 16#000002#,
                         Fmod_Err_Channel_Stolen => 16#000003#,
                         Fmod_Err_Dma => 16#000004#,
                         Fmod_Err_Dsp_Connection => 16#000005#,
                         Fmod_Err_Dsp_Dontprocess => 16#000006#,
                         Fmod_Err_Dsp_Format => 16#000007#,
                         Fmod_Err_Dsp_Inuse => 16#000008#,
                         Fmod_Err_Dsp_Notfound => 16#000009#,
                         Fmod_Err_Dsp_Reserved => 16#00000A#,
                         Fmod_Err_Dsp_Silence => 16#00000B#,
                         Fmod_Err_Dsp_Type => 16#00000C#,
                         Fmod_Err_File_Bad => 16#00000D#,
                         Fmod_Err_File_Couldnotseek => 16#00000E#,
                         Fmod_Err_File_Diskejected => 16#00000F#,
                         Fmod_Err_File_Eof => 16#000010#,
                         Fmod_Err_File_Endofdata => 16#000011#,
                         Fmod_Err_File_Notfound => 16#000012#,
                         Fmod_Err_Format => 16#0000013#,
                         Fmod_Err_Header_Mismatch => 16#000014#,
                         Fmod_Err_Http => 16#000015#,
                         Fmod_Err_Http_Access => 16#000016#,
                         Fmod_Err_Http_Proxy_Auth => 16#000017#,
                         Fmod_Err_Http_Server_Error => 16#000018#,
                         Fmod_Err_Http_Timeout => 16#000019#,
                         Fmod_Err_Initialization => 16#00001A#,
                         Fmod_Err_Initialized => 16#00001B#,
                         Fmod_Err_Internal => 16#00001C#,
                         Fmod_Err_Invalid_Float => 16#00001D#,
                         Fmod_Err_Invalid_Handle => 16#00001E#,
                         Fmod_Err_Invalid_Param => 16#00001F#,
                         Fmod_Err_Invalid_Position => 16#000020#,
                         Fmod_Err_Invalid_Speaker => 16#000021#,
                         Fmod_Err_Invalid_Syncpoint => 16#000022#,
                         Fmod_Err_Invalid_Thread => 16#000023#,
                         Fmod_Err_Invalid_Vector => 16#000024#,
                         Fmod_Err_Maxaudible => 16#000025#,
                         Fmod_Err_Memory => 16#000026#,
                         Fmod_Err_Memory_Cantpoint => 16#000027#,
                         Fmod_Err_Needs3D => 16#000028#,
                         Fmod_Err_Needshardware => 16#000029#,
                         Fmod_Err_Net_Connect => 16#00002A#,
                         Fmod_Err_Net_Socket_Error => 16#00002B#,
                         Fmod_Err_Net_Url => 16#00002C#,
                         Fmod_Err_Net_Would_Block => 16#00002D#,
                         Fmod_Err_Notready => 16#00002E#,
                         Fmod_Err_Output_Allocated => 16#00002F#,
                         Fmod_Err_Output_Createbuffer => 16#000030#,
                         Fmod_Err_Output_Drivercall => 16#000031#,
                         Fmod_Err_Output_Format => 16#000032#,
                         Fmod_Err_Output_Init => 16#000033#,
                         Fmod_Err_Output_Nodrivers => 16#000034#,
                         Fmod_Err_Plugin => 16#000035#,
                         Fmod_Err_Plugin_Missing => 16#000036#,
                         Fmod_Err_Plugin_Resource => 16#000037#,
                         Fmod_Err_Plugin_Version => 16#000038#,
                         Fmod_Err_Record => 16#000039#,
                         Fmod_Err_Reverb_Channelgroup => 16#00003A#,
                         Fmod_Err_Reverb_Instance => 16#00003B#,
                         Fmod_Err_Subsounds => 16#00003C#,
                         Fmod_Err_Subsound_Allocated => 16#00003D#,
                         Fmod_Err_Subsound_Cantmove => 16#00003E#,
                         Fmod_Err_Tagnotfound => 16#00003F#,
                         Fmod_Err_Toomanychannels => 16#000040#,
                         Fmod_Err_Truncated => 16#000041#,
                         Fmod_Err_Unimplemented => 16#000042#,
                         Fmod_Err_Uninitialized => 16#000043#,
                         Fmod_Err_Unsupported => 16#000044#,
                         Fmod_Err_Version => 16#000045#,
                         Fmod_Err_Event_Already_Loaded => 16#000046#,
                         Fmod_Err_Event_Liveupdate_Busy => 16#000047#,
                         Fmod_Err_Event_Liveupdate_Mismatch => 16#000048#,
                         Fmod_Err_Event_Liveupdate_Timeout => 16#000049#,
                         Fmod_Err_Event_Notfound => 16#00004A#,
                         Fmod_Err_Studio_Uninitialized => 16#00004B#,
                         Fmod_Err_Studio_Not_Loaded => 16#00004C#,
                         Fmod_Err_Invalid_String => 16#00004D#,
                         Fmod_Err_Already_Locked => 16#00004E#,
                         Fmod_Err_Not_Locked => 16#00005E#,
                         Fmod_Err_Record_Disconnected => 16#00005F#,
                         Fmod_Err_Toomanysamples => 16#000060#,
                         Fmod_Result_Forceint => 16#65536A#);
end Fmod_Common;
