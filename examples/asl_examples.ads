with Command; use Command;
with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Effects; use Effects;
with Waves; use Waves;
with BLIT;

package ASL_Examples is

   package Full_Demo is
      BPM : constant := 120;

      o : constant Sequencer_Note := No_Seq_Note;
      K : constant Sequencer_Note := (Note => (G, 3), Duration => 3000);
      Z : constant Sequencer_Note := (Note => (G, 3), Duration => 5000);
      B : constant Sequencer_Note := (Note => (G, 3), Duration => 8000);

      Kick_Source : constant Note_Generator_Access :=
        Note_Generator_Access
          (Create_Sequencer
             (16, BPM, 2,
              (K, o, o, K, o, o, K, o, o, o, B, o, o, o, o, o,
               K, o, o, K, o, o, K, o, o, o, K, o, o, o, o, K)));

      Kick : constant access Mixer :=
        Create_Mixer
          ((
           1 => (Create_Sine
                 (Create_Pitch_Gen
                    (0, Kick_Source, Proc => LFO (6.0, 200.0))),
                 0.1),

           2 => (Create_Sine (Create_Pitch_Gen
                 (-24, Kick_Source,
                    Proc => new Attenuator'
                      (Level  => 300.0,
                       Source => Create_ADSR (0, 50, 10000, 0.1, Kick_Source),
                       others => <>))),

                 0.7)
          ), Volume_Mod => Create_ADSR (10, 1000, 200, 0.2, Kick_Source));

      Snare_Seq : constant access Simple_Sequencer :=
        Create_Sequencer
          (Nb_Steps => 16, BPM => BPM, Measures => 4,
           Notes    =>
             (o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o,
              o, o, o, o, K, o, o, o, o, o, o, o, B, o, K, K,
              o, o, o, o, Z, o, o, o, o, o, o, o, K, o, o, o,
              o, o, o, o, K, o, o, K, o, o, Z, o, B, o, Z, o));

      Snare_Source : constant Note_Generator_Access :=
        Note_Generator_Access (Snare_Seq);
      Snare        : constant access Mixer :=
        Create_Mixer
          ((
           1 => (Create_Noise, 0.5),
           2 => (Create_Sine
                 (Create_Pitch_Gen
                    (5, Snare_Source,
                       Proc =>
                          new Attenuator'
                         (Level  => 300.0,
                          Source =>
                            Create_ADSR (0, 200, 10000, 0.5, Snare_Source),
                          others => <>))),
                 0.1)
          ), Volume_Mod => Create_ADSR (0, 100, 100, 0.2, Snare_Source));

      Hat_Seq    : constant access Simple_Sequencer :=
        Create_Sequencer
          (16, BPM,
           Notes => (K, o, K, K, K, o, K, K, K, o, K, K, K, o, K, K));
      Hat_Source : constant Note_Generator_Access :=
        Note_Generator_Access (Hat_Seq);
      Hat        : constant access Mixer :=
        Create_Mixer
          ((
           1 => (Create_Noise, 0.5)
          ), Volume_Mod => Create_ADSR (0, 20, 0, 0.0, Hat_Source));

      SNL : constant Sample_Period := 4000;
      S1  : constant Sequencer_Note := ((C, 4), SNL);
      S2  : constant Sequencer_Note := ((F, 4), SNL);
      S3  : constant Sequencer_Note := ((D_Sh, 4), SNL);
      S4  : constant Sequencer_Note := ((A_Sh, 4), SNL);
      S5  : constant Sequencer_Note := ((G, 4), SNL);
      S6  : constant Sequencer_Note := ((D_Sh, 4), SNL);

      Synth_Seq : constant access Simple_Sequencer :=
        Create_Sequencer
          (8, BPM, 4,
           (S1, S1, S1, S1, S1, S2, S2, S2,
            S3, S3, S3, S3, S3, S4, S4, S4,
            S1, S1, S1, S1, S1, S2, S2, S2,
            S5, S5, S5, S5, S5, S6, S6, S6));

      Synth_Source : constant Note_Generator_Access :=
        Note_Generator_Access (Synth_Seq);

      Synth : constant Generator_Access :=
        Create_Dist
          (Create_LP
             (Create_Mixer
                ((
                 4 => (Create_Sine
                       (Create_Pitch_Gen
                            (-30, Synth_Source)), 0.6),
                 3 => (BLIT.Create_Saw
                       (Create_Pitch_Gen
                            (-24, Synth_Source)), 0.3),
                 2 => (BLIT.Create_Saw
                       (Create_Pitch_Gen
                            (-12, Synth_Source)), 0.3),
                 1 => (BLIT.Create_Saw
                       (Create_Pitch_Gen
                            (-17, Synth_Source)), 0.5))
                ),
              Fixed (200.0,
                Modulator => new Attenuator'
                  (Level  => 1500.0,
                   Source => Create_ADSR (10, 150, 200, 0.005, Synth_Source),
                   others => <>)),
              0.2), 1.00001, 1.5);

      Main_Mixer : constant Generator_Access :=
        Create_Mixer ((
                      (Kick, 0.5),
                      (Snare, 0.7),
                      (Hat, 0.6),
                      (Synth, 0.45)
                     ));
   end Full_Demo;

   package Full_Demo_2 is

      BPM : constant := 120;
      SNL : constant Sample_Period := 4000;

      S1 : constant Sequencer_Note := ((C, 4), SNL);
      S2 : constant Sequencer_Note := ((F, 4), SNL);
      S3 : constant Sequencer_Note := ((D_Sh, 4), SNL);
      S4 : constant Sequencer_Note := ((A_Sh, 4), SNL);
      S5 : constant Sequencer_Note := ((G, 4), SNL);
      S6 : constant Sequencer_Note := ((D_Sh, 4), SNL);

      Synth_Seq : constant Note_Generator_Access :=
        Note_Generator_Access
          (Create_Sequencer
             (8, BPM, 4,
              Notes =>
                (S1, S1, S1, S1, S1, S2, S2, S2,
                 S3, S3, S3, S3, S3, S4, S4, S4,
                 S1, S1, S1, S1, S1, S2, S2, S2,
                 S5, S5, S5, S5, S5, S6, S6, S6)));

      Synth : constant Generator_Access :=
      --  We distort the output signal of the synthetizer with a soft clipper
        Create_Dist
          (Clip_Level => 1.00001,
           Coeff      => 1.5,

           --  The oscillators of the synth are fed to an LP filter
           Source     => Create_LP
             (

              --  We use an ADSR enveloppe to modulate the Cut frequency of the
              --  filter. Using it as the modulator of a Fixed generator allows
              --  us to have a cut frequency that varies between 1700 hz and
              --  200hz.
              Cut_Freq =>
                Fixed
                  (Freq      => 200.0,
                   Modulator => new Attenuator'
                     (Level  => 1500.0,
                      Source => Create_ADSR (10, 150, 200, 0.005, Synth_Seq),
                      others => <>)),

              --  Q is the resonance of the filter, very high values will give
              --  a resonant sound.
              Q        => 0.2,

              --  This is the mixer, receiving the sound of 4 differently tuned
              --  oscillators, 1 sine and 3 saws
              Source   =>
                Create_Mixer
                  (Sources =>
                       (4 => (Create_Sine
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -30, Source => Synth_Seq)),
                              Level => 0.6),
                        3 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -24, Source => Synth_Seq)),
                              Level => 0.3),
                        2 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -12, Source => Synth_Seq)),
                              Level => 0.3),
                        1 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -17, Source => Synth_Seq)),
                              Level => 0.5)))));

      Main_Mixer : constant Generator_Access :=
        Create_Mixer ((1 => (Synth, 0.5)));
   end Full_Demo_2;

end ASL_Examples;
