with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with System;
with Utils; use Utils;

package body Soundio_Output is

   -------------------
   -- Set_Generator --
   -------------------

   procedure Set_Generator (Out_Stream : access SoundIo_Out_Stream;
                            G          : access Generator'Class)
   is
      function Access_To_Address is new Ada.Unchecked_Conversion
        (Source => Generator_Wrapper_Access, Target => System.Address);
   begin
      Out_Stream.User_Data := Access_To_Address
        (new Generator_Wrapper'(G => G, Current_Sample => <>));
   end Set_Generator;

   -------------------
   -- Get_Generator --
   -------------------

   function Get_Generator
     (Out_Stream : access SoundIo_Out_Stream) return Generator_Wrapper_Access
   is
      function Address_To_Access is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Generator_Wrapper_Access);

   begin
      return Address_To_Access (Out_Stream.User_Data);
   end Get_Generator;

   ---------------------
   -- Get_Next_Sample --
   ---------------------

   function Get_Next_Sample
     (G : Generator_Wrapper_Access) return Float
   is
   begin
      if G.Current_Sample = B_Range_T'Last then
         Next_Steps;
         G.G.Next_Samples;
         Sample_Nb := Sample_Nb + Generator_Buffer_Length;
         G.Current_Sample := 0;
      else
         G.Current_Sample := G.Current_Sample + 1;
      end if;

      return Float (G.G.Buffer (G.Current_Sample));
   end Get_Next_Sample;

   Err            : SoundIo_Error;

   procedure Write_Float_Sample is new Write_Sample (Float);

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int)
   is
      Layout            : SoundIo_Channel_Layout renames Out_Stream.Layout;
      Sample_Rate       : Float := Float (Out_Stream.Sample_Rate);
      Seconds_Per_Frame : Float := 1.0 / Sample_Rate;
      Areas             : SoundIo_Channel_Area_Ptr;
      Frames_Left       : int := Frame_Count_Max;
      Sample            : Float;
      use Soundio_Channel_Area_Ptrs;
   begin

      while Frames_Left > 0 loop
         declare
            Frame_Count : int := Frames_Left;
         begin
            Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);
            exit when Frame_Count = 0;

            for Frame in 0 .. Frame_Count - 1 loop
               Sample := Get_Next_Sample (Get_Generator (Out_Stream));

               --  TODO: Write Get_Area function that returns the Area at index
               --  Channel
               for Channel in 0 .. Layout.Channel_Count - 1 loop
                  Write_Float_Sample
                    (Get_Area (Areas, Channel), Frame, Sample);
               end loop;
            end loop;

            Err := Outstream_End_Write (Out_Stream);
            Frames_Left := Frames_Left - Frame_Count;
         end;
      end loop;
   end Write_Callback;

end Soundio_Output;
