with Soundio; use Soundio;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Soundio_Output; use Soundio_Output;
with Simple_Demo;

procedure Soundio_Example is

   IO                   : access Soundio.SoundIo := Create;
   Default_Device_Index : int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Err                  : SoundIo_Error;
begin
   --  SoundIO connection boilerplate
   Err := Connect (IO);
   Flush_Events (IO);
   Default_Device_Index := Default_Output_Device_Index (IO);
   Device := Get_Output_Device (IO, Default_Device_Index);
   Out_Stream := Outstream_Create (Device);
   Out_Stream.Format := Format_Float32NE;
   Out_Stream.Write_Callback := Soundio_Output.Write_Callback'Access;

   --  Set the main generator for the audio stream.
   Set_Generator (Out_Stream => Out_Stream, G => Simple_Demo.Main_Mixer);
   Err := Outstream_Open (Out_Stream);
   Err := Outstream_Start (Out_Stream);

   Put_Line ("Backend used : " & IO.Current_Backend'Img);
   Put_Line ("BACKEND = " & IO.Current_Backend'Image);

   Play (Out_Stream);
   loop
      Wait_Events (IO);
   end loop;

   pragma Warnings (Off, "Unreachable");
   Outstream_Destroy (Out_Stream);
   Device_Unref (Device);
   Destroy (IO);
end Soundio_Example;
