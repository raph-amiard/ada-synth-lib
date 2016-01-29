with Utils; use Utils;
with Waves; use Waves; with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Command; use Command;
with BLIT; use BLIT;

package Simple_Demo_3 is

   BPM : constant := 120;
   SNL : constant Sample_Period := 4000;

   S1 : constant Sequencer_Note := ((C, 4), SNL);
   S2 : constant Sequencer_Note := ((F, 4), SNL);
   S3 : constant Sequencer_Note := ((D_Sh, 4), SNL);
   S4 : constant Sequencer_Note := ((A_Sh, 4), SNL);
   S5 : constant Sequencer_Note := ((G, 4), SNL);
   S6 : constant Sequencer_Note := ((D_Sh, 4), SNL);

   o : constant Sequencer_Note := No_Seq_Note;
   K : constant Sequencer_Note := (Note => (G, 3), Duration => 3000);

   Synth_Seq : constant access Simple_Sequencer :=
     Create_Sequencer
       (16, BPM, 4,
        Notes =>
          (S1, S1, S1, S1, S1, S2, S2, S2, S5, S5, S5, S5, S5, S6, S6, S6,
           S3, S3, S3, S3, S3, S4, S4, S4, S5, S5, S5, S5, S5, S6, S6, S6,
           S1, S1, S1, S1, S1, S2, S2, S2, S5, S5, S5, S5, S5, S6, S6, S6,
           S5, S5, S5, S5, S5, S6, S6, S6, S5, S5, S5, S5, S5, S6, S6, S6));

   Synth_Seq_2 : constant access Simple_Sequencer :=
     Create_Sequencer
       (8, BPM, 8,
        Notes =>
          (S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1,
           S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3, S3,
           S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2, S2,
           S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1, S1));

   Synth_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq);

   Synth_Source_2 : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq_2);

   Synth : constant access Disto :=
     Create_Dist
       (Clip_Level => 1.00001,
        Coeff      => 1.5,
        Source     => Create_LP
          (
           Cut_Freq =>
             Fixed
               (Freq      => 200.0,
                Modulator =>
                  Generator_Access (Create_Mixer
                    (Saturate => False,
                     Sources =>
                       (1 =>
                          (Create_ADSR
                               (3, 75, 50, 0.005, Synth_Source),
                           Level => 800.0),
                        2 =>
                          (LFO (0.1, 400.0), 1.0))))
               ),
           Q => 0.2,
           Source =>
             Create_Mixer
               (Sources =>
                    (1 => (BLIT.Create_Saw
                           (Create_Pitch_Gen
                              (Rel_Pitch => -33, Source => Synth_Source)),
                           Level => 0.6),
                     2 => (BLIT.Create_Saw
                           (Create_Pitch_Gen
                              (Rel_Pitch => -33,
                               Source => Synth_Source,
                               Proc => Generator_Access (Fixed (2.0)))),
                           Level => 0.6),
                     3 => (Create_Sine
                           (Create_Pitch_Gen
                              (Rel_Pitch => -33, Source => Synth_Source)),
                           Level => 0.6)
                    ),
                Env => Create_ADSR
                  (3, 100, 100, 0.2, Synth_Source))
          ));

   Synth_2 : constant access Low_Pass_Filter :=
     Create_LP
       (Cut_Freq =>
          Fixed
            (Freq      => 300.0,
             Modulator =>
               Generator_Access (Create_Mixer
                 (Saturate => False,
                  Sources =>
                    (1 =>
                       (Create_ADSR
                            (3, 75, 50, 0.005, Synth_Source_2),
                        Level => 1100.0),
                     2 =>
                       (LFO (0.1, 400.0), 1.0))))
            ),
        Q => 0.7,
        Source =>
          Create_Mixer
            (Sources =>
               (1 => (BLIT.Create_Square
                      (Create_Pitch_Gen
                           (Rel_Pitch => -14, Source => Synth_Source_2)),
                      Level => 0.6),
                2 => (BLIT.Create_Square
                      (Create_Pitch_Gen
                           (Rel_Pitch => -14,
                            Source => Synth_Source_2,
                            Proc => Generator_Access (Fixed (4.0)))),
                      Level => 0.6),
                3 => (BLIT.Create_Square
                      (Create_Pitch_Gen
                           (Rel_Pitch => -21, Source => Synth_Source_2)),
                      Level => 0.6),
                4 => (BLIT.Create_Square
                      (Create_Pitch_Gen
                           (Rel_Pitch => -21,
                            Source => Synth_Source_2,
                            Proc => Generator_Access (Fixed (4.0)))),
                      Level => 0.6))));

   H : constant Sequencer_Note := (Note => (G, 3), Duration => 100);
   HH : constant Sequencer_Note := (Note => (G, 3), Duration => 500);

   Hat_Seq : constant access Simple_Sequencer :=
     Create_Sequencer
       (16, BPM,
        Notes => (H, o, H, H, H, o, H, H, HH, o, H, H, H, o, H, H));
   Hat_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Hat_Seq);
   Hat : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Noise, 0.5)
       ), Env => Create_ADSR (5, 20, 20, 0.00001, Hat_Source));

   Kick_Source : constant Note_Generator_Access :=
     Note_Generator_Access
       (Create_Sequencer
          (16, BPM, 4,
           (K, o, o, K, o, o, o, o, K, o, K, o, o, K, o, o,
            K, o, o, K, o, o, o, o, K, o, K, o, o, o, o, K,
            K, o, o, K, o, o, o, o, K, o, K, o, o, K, o, o,
            K, o, o, K, o, o, o, o, K, o, K, o, o, o, o, K)));

   Kick : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Sine
              (Create_Pitch_Gen
                 (0, Kick_Source)),
              0.1),

        2 => (Create_Sine (Create_Pitch_Gen
              (-24, Kick_Source,
                 Proc => new Attenuator'
                   (Level => 300.0,
                    Source => Create_ADSR (0, 50, 100, 0.1, Kick_Source),
                    others => <>))),

              0.7)
       ), Env => Create_ADSR (1, 100, 100, 0.2, Kick_Source));

   Main : constant access Generator'Class :=
     Create_Mixer ((1 => (Hat,         0.7),
                    2 => (Kick,        0.7),
                    3 => (Synth_2, 0.05),
                    4 => (Synth,       0.7)));


end Simple_Demo_3;
