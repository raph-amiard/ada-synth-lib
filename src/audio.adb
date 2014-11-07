with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Utils; use Utils;
with Waves; use Waves;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Command; use Command;

procedure Audio is

   Int_Smp : Short_Integer := 0;
   function Sample_To_Uint16 is new Sample_To_Int (Short_Integer);
   Ignore : Integer;
   pragma Unreferenced (Ignore);

   BPM : constant := 120;

   Kick_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (16, BPM, 2);
   Kick_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Kick_Seq);
   Kick_VCA : constant Signal_Processor_Access :=
     VCA (Create_ADSR (10, 1000, 200, 0.2, Kick_Source));
   Kick : constant access Mixer :=
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

   Snare_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (16, BPM, 4);
   Snare_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Snare_Seq);
   Snare_ADSR : constant Generator_Access :=
     Create_ADSR (0, 100, 100, 0.2, Snare_Source);
   Snare_VCA : constant Signal_Processor_Access := VCA (Snare_ADSR);
   Snare : constant access Mixer :=
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

   Hat_Seq : constant access Simple_Sequencer := Create_Sequencer (16, BPM);
   Hat_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Hat_Seq);
   Hat_VCA : constant Signal_Processor_Access :=
     VCA (Create_ADSR (0, 20, 0, 0.0, Hat_Source));
   Hat : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Chain
              (Create_Noise, (1 => Hat_VCA)), 0.5)
       ));

   Synth_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (8, BPM, 4);
   Synth_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq);
   Synth_VCA : constant Signal_Processor_Access :=
     VCA (Create_ADSR (0, 100, 200, 0.1, Synth_Source));
   Synth_LFO : constant Signal_Processor_Access := VCA (LFO (6.0, 0.5));
   Synth : constant access Chain :=
     Create_Chain
       (Create_Mixer
          ((
           1 => (Create_Chain
                 (Create_Square (Create_Pitch_Gen (-24, Synth_Source)),
                    (1 => Synth_VCA, 2 => Synth_LFO)), 0.8),
           2 => (Create_Chain
                 (Create_Square (Create_Pitch_Gen (7, Synth_Source)),
                    (1 => Synth_VCA, 2 => Synth_LFO)), 0.5)
--             3 => (Create_Chain
--                   (Create_Square (Create_Pitch_Gen (3, Synth_Source)),
--                      (1 => Synth_VCA, 2 => Synth_LFO)), 0.5),
--             3 => (Create_Chain
--                   (Create_Square (Create_Pitch_Gen (19, Synth_Source)),
--                      (1 => Synth_VCA, 2 => Synth_LFO)), 0.5)
          )),
        (1 => Create_LP
           (Fixed (1_500.0, Proc => LFO (6.0, 2000.0)), 0.7)));

   Main_Mixer : constant access Mixer :=
     Create_Mixer ((
                   (Kick, 0.5),
                   (Snare, 0.7),
                   (Hat, 0.6),
                   (Synth, 0.3)
                  ));

   o : constant Sequencer_Note := No_Seq_Note;
   K : constant Sequencer_Note := ((G, 3), 3000);
   Z : constant Sequencer_Note := ((G, 3), 5000);
   B : constant Sequencer_Note := ((G, 3), 8000);

   SNL : constant Period := 4000;

   S1 : constant Sequencer_Note := ((C, 4), SNL);
   S2 : constant Sequencer_Note := ((F, 4), SNL);
   S3 : constant Sequencer_Note := ((D_Sh, 4), SNL);
   S4 : constant Sequencer_Note := ((A_Sh, 4), SNL);
   S5 : constant Sequencer_Note := ((G, 4), SNL);
   S6 : constant Sequencer_Note := ((D_Sh, 4), SNL);
begin
   Kick_Seq.Notes  := (K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o,
                       K, o, o, K, o, o, K, o, o, o, K, o, o, o, o, K);

   Snare_Seq.Notes := (o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o,
                       o, o, o, o, K, o, o, o, o, o, o, o, B, o, K, K,
                       o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o,
                       o, o, o, o, K, o, o, K, o, o, Z, o, B, o, Z, o);

   Hat_Seq.Notes   := (K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K);

   Synth_Seq.Notes :=
     (S1, S1, S1, S1, S1, S2, S2, S2,
      S3, S3, S3, S3, S3, S4, S4, S4,
      S1, S1, S1, S1, S1, S2, S2, S2,
      S5, S5, S5, S5, S5, S6, S6, S6);

   Put_Line (Standard_Error, Note_To_Freq ((A, 5))'Img);

   loop
      Int_Smp := Sample_To_Uint16 (Main_Mixer.Next_Sample);
      Sample_Nb := Sample_Nb + 1;
      Ignore := GNAT.OS_Lib.Write
        (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
   end loop;

end Audio;
