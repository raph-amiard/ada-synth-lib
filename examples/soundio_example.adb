with Ada.Text_IO;        use Ada.Text_IO;
with Interfaces.C;       use Interfaces.C;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Soundio;              use Soundio;
with Soundio_Output;       use Soundio_Output;

with ASL_Examples;

procedure Soundio_Example is

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Run an example for ada-synth-lib");

      type Example_Kind is (Full_Demo);

      package Example is new Parse_Enum_Option
        (Parser,
         "-e", "--example", "Which example to run",
         Arg_Type    => Example_Kind, Default_Val => Full_Demo);

      function Generator return Generator_Access is
        (case Arg.Example.Get is
            when Full_Demo => ASL_Examples.Full_Demo.Main_Mixer);
   end Arg;

   IO                   : constant access Soundio.SoundIo := Create;
   Default_Device_Index : int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Dummy                : SoundIo_Error;

begin
   if Arg.Parser.Parse then

      --  SoundIO connection boilerplate
      Dummy := Connect (IO);
      Flush_Events (IO);
      Default_Device_Index := Default_Output_Device_Index (IO);
      Device := Get_Output_Device (IO, Default_Device_Index);
      Out_Stream := Outstream_Create (Device);
      Out_Stream.Format := Format_Float32NE;
      Out_Stream.Write_Callback := Soundio_Output.Write_Callback'Access;

      --  Set the main generator for the audio stream.
      Set_Generator
        (Out_Stream => Out_Stream, G => Arg.Generator);
      Dummy := Outstream_Open (Out_Stream);
      Dummy := Outstream_Start (Out_Stream);

      Put_Line ("Backend used : " & IO.current_backend'Img);

      Play (Out_Stream);
      loop
         Wait_Events (IO);
      end loop;

      pragma Warnings (Off, "Unreachable");
      Outstream_Destroy (Out_Stream);
      Device_Unref (Device);
      Destroy (IO);
   end if;
end Soundio_Example;
