with Soundio;              use Soundio;
with Interfaces.C;         use Interfaces.C;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Ring_Buffer;

package Soundio_Output is

   type State is (Play, Stop);

   type Soundio_Mode is (Callback, Buffer);

   package FRB
   is new Ring_Buffer (Float, Checks => True, Default_Value => 0.0);

   type Soundio_User_Data (Mode : Soundio_Mode) is record
      S              : State;
      G              : access Generator'Class;
      G_Buffer       : Generator_Buffer;
      Current_Sample : Buffer_Range_Type := Buffer_Range_Type'Last;

      case Mode is
         when Callback =>
            null;
         when Buffer =>
            Ring_Buf       : FRB.Ring_Buffer;
      end case;
   end record;
   type User_Data_Access is access all Soundio_User_Data;

   procedure Play
     (Out_Stream : access SoundIo_Out_Stream);

   procedure Stop
     (Out_Stream : access SoundIo_Out_Stream);

   procedure Set_Generator
     (Out_Stream : access SoundIo_Out_Stream;
      G          : access Generator'Class);

   procedure Set_Ring_Buffer
     (Out_Stream : access SoundIo_Out_Stream;
      Ring_Buf   : FRB.Ring_Buffer;
      G          : access Generator'Class);

   function Get_Generator
     (Out_Stream : access SoundIo_Out_Stream) return User_Data_Access;

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Callback);

   procedure Write_Samples
     (Out_Stream : access SoundIo_Out_Stream;
      Max_Nb_Samples : Natural := Natural (Buffer_Range_Type'Last));

   function Drift_Level return int;

end Soundio_Output;
