with Ada.Text_IO; use Ada.Text_IO;

package body MIDI_Synthesizer is
   procedure Update_ADSR (Self : in out Synthesizer) is
      Value : Float;
   begin
      for I in Self.ADSR_Config'Range loop
         Value :=
           Float (Self.ADSR_Config (I).Value) * Self.ADSR_Config (I).Factor;
         Self.Env0.Set_Value (I, Value);
         --Put_Line (Standard_Error, "index: " & Integer'Image(I) & " = " & Float'Image(Value));
      end loop;
   end Update_ADSR;

   function Create_Synthesizer return access Synthesizer is
      Ret    : constant access Synthesizer := new Synthesizer;
      Base   : Float                       := 8.1757989156;  -- MIDI note C1 0
      Freq0  : access Fixed_Gen            := Fixed;
      Gen0   : access Sine_Generator       := Create_Sine (Freq0);
      Env0   : access ADSR := Create_ADSR (5, 50, 800, 0.5, null);
      Mixer0 : access Mixer := Create_Mixer ((0 => (Gen0, 0.5)), Env => Env0);
   begin
      Ret.MIDI_Parser := Create_Parser (Ret);
      for I in Ret.MIDI_Notes'Range loop
         Ret.MIDI_Notes (I) := Base;
         Base               := Base * 1.059463094359;  -- 2^(1/12)
      end loop;
      Ret.Freq0       := Freq0;
      Ret.Env0        := Env0;
      Ret.Mixer0      := Mixer0;
      Ret.ADSR_Config :=
        ((16#66#, 1, 1, 100, 0.05),
         (16#67#, 1, 1, 100, 0.05),
         (16#68#, 10, 1, 100, 0.05),
         (16#69#, 20, 1, 100, 0.05));
      Ret.Update_ADSR;
      return Ret;
   end Create_Synthesizer;

   procedure Parse_MIDI_Byte
     (Self     : in out Synthesizer;
      Received : in     Unsigned_8)
   is
   begin
      Self.MIDI_Parser.Parse (Received);
   end Parse_MIDI_Byte;

   procedure Note_On
     (Self     : in out Synthesizer;
      Channel  : in     Unsigned_8;
      Note     : in     Unsigned_8;
      Velocity : in     Unsigned_8)
   is
   begin
      Self.Env0.Gate_On;
      Self.Freq0.Set_Value (0, Self.MIDI_Notes (Integer (Note mod 128)));
   end Note_On;

   procedure Note_Off
     (Self     : in out Synthesizer;
      Channel  : in     Unsigned_8;
      Note     : in     Unsigned_8;
      Velocity : in     Unsigned_8)
   is
   begin
      Self.Env0.Gate_Off;
   end Note_Off;

   -- Testing with an Arturia Beatstep Pro:
   -- The 16 rotary encoders are configured for relative mode.
   -- For increasing, the following sequence is sent:
   -- B0 66 40
   -- B0 66 41
   -- For decreasing the following sequence:
   -- B0 66 40
   -- B0 66 3F
   -- where 66 specifies the encoder number, and goes up to 75 (all value hex).
   procedure Control_Change
     (Self              : in out Synthesizer;
      Channel           : in     Unsigned_8;
      Controller_Number : in     Unsigned_8;
      Controller_Value  : in     Unsigned_8)
   is
      Encoder : Integer := 0;
      Incr    : Integer := 0;
   begin
      if Controller_Value = 16#41# then
         Incr := 1;
      elsif Controller_Value = 16#3f# then
         Incr := -1;
      end if;
      if Incr /= 0 then
         for I in Self.ADSR_Config'Range loop
            if Self.ADSR_Config (I).Controller_Index = Controller_Number then
               Self.ADSR_Config (I).Value := Self.ADSR_Config (I).Value + Incr;
               if Self.ADSR_Config (I).Value <
                 Self.ADSR_Config (I).Min_Value
               then
                  Self.ADSR_Config (I).Value := Self.ADSR_Config (I).Min_Value;
               end if;
               if Self.ADSR_Config (I).Value >
                 Self.ADSR_Config (I).Max_Value
               then
                  Self.ADSR_Config (I).Value := Self.ADSR_Config (I).Max_Value;
               end if;
               Self.Update_ADSR;
               exit;
            end if;
         end loop;
      end if;
   end Control_Change;

end MIDI_Synthesizer;
