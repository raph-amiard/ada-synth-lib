with Soundio; use Soundio;
with Interfaces.C; use Interfaces.C;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

package Soundio_Output is

   type Generator_Wrapper is record
      G              : access Generator'Class;
      Current_Sample : B_Range_T := B_Range_T'Last;
   end record;
   type Generator_Wrapper_Access is access all Generator_Wrapper;

   procedure Set_Generator (Out_Stream : access SoundIo_Out_Stream;
                            G          : access Generator'Class);

   function Get_Generator
     (Out_Stream : access SoundIo_Out_Stream) return Generator_Wrapper_Access;

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Callback);

end Soundio_Output;
