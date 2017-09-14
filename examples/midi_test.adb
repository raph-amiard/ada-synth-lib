-- This sample program receives MIDI events from a keyboard and from
-- rotary encoders and outputs audio, like a synthesizer.
-- Call it like this from Linux:
--
-- amidi -p "hw:1,0,0" -r >(./obj/midi_test | aplay -f S16_LE -c1 -r44100 --buffer-size=4096)
--
-- The BASH syntax ">(program)" creates a temporary FIFO file, because amidi
-- needs a file where it writes the received MIDI events. In case of problems,
-- you can also create a named FIFO with "mkfifo", then start amidi in the
-- background writing to this file, and then the midi_test program like this:
--
-- cat midi | ./obj/midi_test | aplay -f S16_LE -c1 -r44100 --buffer-size=4096)
--
-- where "midi" is the named FIFO file. If it keeps playing a tone when you stop
-- the program with ctrl-c, try this command:
--
-- killall amidi aplay
--
-- You can see the list of available MIDI devices with "amidi -l".
-- For testing it is useful to use the AMIDI "--dump" option.
-- For lower latency, you might need to change the Linux pipe size:
--
-- sudo sysctl fs.pipe-max-size=4096

with GNAT.OS_Lib;
with Interfaces;       use Interfaces;
with MIDI_Synthesizer; use MIDI_Synthesizer;
with Write_To_Stdout_Once;

procedure MIDI_Test is
   Data   : Unsigned_8;
   Ignore : Integer;

   Main_Synthesizer : access Synthesizer'Class := Create_Synthesizer;

   task Main_Task is
      entry Data_Received (Data : in Unsigned_8);
   end Main_Task;

   task body Main_Task is
   begin
      loop
         select
            accept Data_Received (Data : in Unsigned_8) do
               Main_Synthesizer.Parse_MIDI_Byte (Data);
            end Data_Received;
         else
            Write_To_Stdout_Once (Main_Synthesizer.Mixer0);
         end select;
      end loop;
   end Main_Task;

begin
   loop
      Ignore :=
        GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Data'Address, Data'Size / 8);
      Main_Task.Data_Received (Data);
   end loop;
end MIDI_Test;
