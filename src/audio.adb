with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Utils; use Utils;
with Waves; use Waves;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Command; use Command;
with Ada.Finalization; use Ada.Finalization;

procedure Audio is

   Int_Smp : Short_Integer := 0;
   function Sample_To_Uint16 is new Sample_To_Int (Short_Integer);
   Ignore : Integer;

   Kick_Seq : access Simple_Sequencer := Create_Sequencer (16, 120);
   Kick_Source : Note_Generator_Access := Note_Generator_Access (Kick_Seq);
   Kick_VCA : Signal_Processor_Access := VCA (Create_ADSR (10, 1000, 200, 0.2, Kick_Source));
   Kick : access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Chain
              (Create_Sine
                 (Create_Pitch_Gen (0, Kick_Source, Proc => LFO (6.0, 200.0))),
                 (1 => Kick_VCA)),
              0.1),

        2 => (Create_Chain
              (Create_Sine (Create_Pitch_Gen
                 (-24, Kick_Source,
                    Proc =>
                      Create_Chain
                        (Create_ADSR (0, 50, 10000, 0.1, Kick_Source),
                         (0 => new Attenuator'(Level => 300.0))))),
                 (1 => Kick_VCA)),

              0.7)
       ));

   Snare_Seq : access Simple_Sequencer := Create_Sequencer (16, 120);
   Snare_Source : Note_Generator_Access := Note_Generator_Access (Snare_Seq);
   Snare_VCA : Signal_Processor_Access := VCA (Create_ADSR (0, 100, 100, 0.2, Snare_Source));
   Snare : access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Chain
              (Create_Noise, (1 => Snare_VCA)), 0.5),
        2 => (Create_Chain
              (Create_Sine
                 (Create_Pitch_Gen
                    (12, Snare_Source,
                       Proc =>
                         Create_Chain
                           (Create_ADSR (0, 200, 10000, 0.5, Snare_Source),
                            (0 => new Attenuator'(Level => 300.0))))),
                 (1 => Snare_VCA)),
              0.1)
       ));

   Hat_Seq : access Simple_Sequencer := Create_Sequencer (16, 120);
   Hat_Source : Note_Generator_Access := Note_Generator_Access (Hat_Seq);
   Hat_VCA : Signal_Processor_Access := VCA (Create_ADSR (0, 20, 0, 0.0, Hat_Source));
   Hat : access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Chain
              (Create_Noise, (1 => Hat_VCA)), 0.5)
       ));

   Main_Mixer : access Mixer :=
     Create_Mixer ((1 => (Kick, 0.5),
                    2 => (Snare, 0.5),
                    3 => (Hat, 0.5)));

   o : Sequencer_Note := No_Seq_Note;
   K : Sequencer_Note := ((A, 4), 8000);
begin
   Kick_Seq.Notes  := (K, o, o, K, o, o, K, o, o, o, K, o, o, o, o, o);
   Snare_Seq.Notes := (o, o, o, o, K, o, o, o, o, o, o, o, K, o, o, o);
   Hat_Seq.Notes   := (K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K);

   Put_Line (Standard_Error, Note_To_Freq ((A, 5))'Img);

   loop
      Int_Smp := Sample_To_Uint16 (Main_Mixer.Next_Sample);
      Sample_Nb := Sample_Nb + 1;
      Ignore := GNAT.OS_Lib.Write
        (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
   end loop;

end Audio;
