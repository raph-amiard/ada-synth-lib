with Ada.Text_IO;        use Ada.Text_IO;
with Interfaces.C;       use Interfaces.C;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Soundio;              use Soundio;
with Soundio_Output;       use Soundio_Output;

with ASL_Examples;
with Write_To_Stdout;

procedure Example is

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Run an example for ada-synth-lib");

      type Example_Kind is (Full_Demo, Full_Demo_2, Trippy_Demo, Simple_Sine,
                            Programmatic_Drums, Poly_Synth);
      type Backend_Kind is (Pulse_Audio, Stdout);

      package Example is new Parse_Enum_Option
        (Parser,
         "-e", "--example", "Which example to run",
         Arg_Type    => Example_Kind, Default_Val => Full_Demo);

      package Backend is new Parse_Enum_Option
        (Parser,
         "-b", "--backend", "Which backend to use",
         Arg_Type    => Backend_Kind, Default_Val => Pulse_Audio);

      function Generator return Generator_Access is
        (case Arg.Example.Get is
            when Full_Demo => ASL_Examples.Full_Demo.Main_Mixer,
            when Full_Demo_2 => ASL_Examples.Full_Demo_2.Main_Mixer,
            when Trippy_Demo => ASL_Examples.Trippy_Demo.Main,
            when Simple_Sine => ASL_Examples.Simple_Sine.Main,
            when Programmatic_Drums => ASL_Examples.Programmatic_Drums.Main,
            when Poly_Synth => ASL_Examples.Poly_Synth.Main
        );
   end Arg;

   IO                   : constant access Soundio.SoundIo := Create;
   Default_Device_Index : int;
   Device               : access SoundIo_Device;
   Out_Stream           : access SoundIo_Out_Stream;
   Dummy                : SoundIo_Error;

begin
   if Arg.Parser.Parse then
      case Arg.Backend.Get is
      when Arg.Pulse_Audio =>
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
      when Arg.Stdout =>
         Write_To_Stdout (Arg.Generator);
      end case;
   end if;
end Example;
