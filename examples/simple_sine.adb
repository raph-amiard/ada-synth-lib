with Waves; use Waves;
with Write_To_Stdout;
with Command; use Command;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;

procedure Simple_Sine is
   pragma Suppress (Accessibility_Check);
   BPM   : Natural := 15;
   Notes : Notes_Array :=
     To_Seq_Notes ((C, G, F, G, C, G, F, A, C, G, F, G, C, G, F, G), 400, 4);

   function Simple_Synth
     (S    : Note_Generator_Access; Tune : Integer := 0; Decay : Integer)
      return access Mixer
   is
     (Create_Mixer
        ((0 => (Create_Sine (Create_Pitch_Gen (Tune, S)), 0.5)),
         Volume_Mod => Create_ADSR (5, 50, Decay, 0.5, S)));

   Volume     : Float   := 0.9;
   Decay      : Integer := 800;
   Seq        : Simple_Sequencer_Access;
   Sine_Gen   : access Mixer;
   Main       : constant access Mixer := Create_Mixer (No_Generators);
begin
   for I in -3 .. 1 loop
      Seq      := Create_Sequencer (16, BPM, 1, Notes);
      Sine_Gen := Simple_Synth (Note_Generator_Access (Seq), I * 12, Decay);
      Main.Add_Generator (Sine_Gen, Volume);
      BPM    := BPM * 2;
      Volume := Volume / 1.8;
      Decay  := Decay / 2;
   end loop;

   Write_To_Stdout (Main);
end Simple_Sine;
