with Command; use Command;
with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Effects; use Effects;
with Waves; use Waves;
with BLIT;
with Polyphony;            use Polyphony;

package ASL_Examples is

   pragma Elaborate_Body;

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

   package Trippy_Demo is

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

      Synth : constant Generator_Access :=
        Create_Dist
          (Clip_Level => 1.00001,
           Coeff      => 1.5,
           Source     => Create_LP
             (
              Cut_Freq =>
                Fixed
                  (Freq      => 200.0,
                   Modulator =>
                     Create_Mixer
                       (Saturate => False,
                        Sources  =>
                          (1 =>
                             (Create_ADSR (3, 75, 50, 0.005, Synth_Source),
                              Level => 800.0),
                           2 => (LFO (0.1, 400.0), 1.0)))
                  ),
              Q        => 0.2,
              Source   =>
                Create_Mixer
                  (Sources    =>
                       (1 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -33, Source => Synth_Source)),
                              Level => 0.6),
                        2 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -33,
                                  Source    => Synth_Source,
                                  Proc      => Fixed (2.0))),
                              Level => 0.6),
                        3 => (Create_Sine
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -33, Source => Synth_Source)),
                              Level => 0.6)
                       ),
                   Volume_Mod => Create_ADSR
                     (3, 100, 100, 0.2, Synth_Source))
             ));

      Synth_2 : constant Generator_Access :=
        Create_LP
          (Cut_Freq =>
             Fixed
               (Freq      => 1500.0,
                Modulator =>
                  Create_Mixer
                    (Saturate => False,
                     Sources  =>
                       (1 =>
                          (Create_ADSR
                               (3, 75, 500, 0.5, Synth_Source_2),
                           Level => 800.0),
                        2 =>
                          (LFO (0.1, 400.0), 1.0)))
               ),
           Q        => 0.7,
           Source   =>
             Create_Mixer
               (Sources =>
                  (1 => (BLIT.Create_Square
                         (Create_Pitch_Gen
                              (Rel_Pitch => -14, Source => Synth_Source_2)),
                         Level => 0.6),
                   2 => (BLIT.Create_Square
                         (Create_Pitch_Gen
                              (Rel_Pitch => -14,
                               Source    => Synth_Source_2,
                               Proc      => Fixed (2.0))),
                         Level => 0.6),
                   3 => (BLIT.Create_Square
                         (Create_Pitch_Gen
                              (Rel_Pitch => -21, Source => Synth_Source_2)),
                         Level => 0.6),
                   4 => (BLIT.Create_Square
                         (Create_Pitch_Gen
                              (Rel_Pitch => -21,
                               Source    => Synth_Source_2,
                               Proc      => Fixed (-2.0))),
                         Level => 0.6))));

      H  : constant Sequencer_Note := (Note => (G, 3), Duration => 100);
      HH : constant Sequencer_Note := (Note => (G, 3), Duration => 500);

      Hat_Seq    : constant access Simple_Sequencer :=
        Create_Sequencer
          (16, BPM,
           Notes => (H, o, H, H, H, o, H, H, HH, o, H, H, H, o, H, H));
      Hat_Source : constant Note_Generator_Access :=
        Note_Generator_Access (Hat_Seq);
      Hat        : constant Generator_Access :=
        Create_Mixer
          ((
           1 => (Create_Noise, 0.5)
          ), Volume_Mod => Create_ADSR (5, 20, 20, 0.00001, Hat_Source));

      Kick_Source : constant Note_Generator_Access :=
        Note_Generator_Access
          (Create_Sequencer
             (16, BPM, 4,
              (K, o, o, K, o, o, o, o, K, o, K, o, o, K, o, o,
               K, o, o, K, o, o, o, o, K, o, K, o, o, o, o, K,
               K, o, o, K, o, o, o, o, K, o, K, o, o, K, o, o,
               K, o, o, K, o, o, o, o, K, o, K, o, o, o, o, K)));

      Kick : constant Generator_Access :=
        Create_Mixer
          ((
           1 => (Create_Sine (Create_Pitch_Gen (-12, Kick_Source)), 0.0),

           2 => (Create_Sine (Create_Pitch_Gen
                 (-28, Kick_Source,
                    Proc => new Attenuator'
                      (Level  => 500.0,
                       Source => Create_ADSR (1, 30, 260, 0.15, Kick_Source),
                       others => <>))),

                 0.7)
          ), Volume_Mod => Create_ADSR (1, 100, 500, 0.5, Kick_Source));

      Main : constant Generator_Access :=
        Create_Mixer ((1 => (Hat,         0.7),
                       2 => (Kick,        0.7),
                       3 => (Synth_2,     0.06),
                       4 => (Synth,       0.9)));
   end Trippy_Demo;

   package Simple_Sine is
      Sine_Gen : constant Generator_Access :=
        Create_Sine (Fixed (200.0, LFO (6.0, 30.0)));

      Main     : constant Generator_Access :=
        Create_Mixer ((1 => (Sine_Gen, 0.7)));
   end Simple_Sine;

   package Programmatic_Drums is
      Main       : constant Generator_Access;
   private
      BPM   : Natural := 15;
      Notes : Notes_Array :=
        To_Seq_Notes
          ((C, G, F, G, C, G, F, A, C, G, F, G, C, G, F, G), 400, 4);

      function Simple_Synth
        (S    : Note_Generator_Access; Tune : Integer := 0; Decay : Integer)
         return Mixer_Access
      is
        (Create_Mixer
           ((0 => (Create_Sine (Create_Pitch_Gen (Tune, S)), 0.5)),
            Volume_Mod => Create_ADSR (5, 50, Decay, 0.5, S)));

      Volume     : Float   := 0.9;
      Decay      : Integer := 800;
      Seq        : Simple_Sequencer_Access;
      Sine_Gen   : access Mixer;
      Mixer      : constant Mixer_Access := Create_Mixer (No_Generators);
      Main       : constant Generator_Access := Generator_Access (Mixer);

      procedure Init;
   end Programmatic_Drums;

   package Poly_Synth is
      BPM : constant := 120;

      SNL : constant Sample_Period := 4000;
      S1  : constant Sequencer_Note := ((C, 3), SNL);
      S2  : constant Sequencer_Note := ((F, 4), SNL);
      S3  : constant Sequencer_Note := ((D_Sh, 4), SNL);
      S4  : constant Sequencer_Note := ((A_Sh, 4), SNL);
      S5  : constant Sequencer_Note := ((G, 4), SNL);
      S6  : constant Sequencer_Note := ((D_Sh, 4), SNL);

      function Create_Voice
        (Note_Gen : Note_Generator_Access) return Generator_Access
      is
        (Create_LP
           (Create_Mixer
                ((1 => (BLIT.Create_Saw (Create_Pitch_Gen (0, Note_Gen)), 0.5),
                  2 => (BLIT.Create_Square
                         (Create_Pitch_Gen (0, Note_Gen)), 0.5)),
                 Volume_Mod => Create_ADSR (100, 1000, 100, 0.2, Note_Gen)),
            Cut_Freq => Fixed
              (200.0,
               Modulator => new Attenuator'
                 (Level  => 4500.0,
                  Source => Create_ADSR
                    (10, 1500, 200, 0.005, Note_Gen),
                  others => <>)),
            Q        => 0.4));

      function Chord
        (Base : Note_T; Base_Time : Sample_Period) return Note_Array
      is
        ((Base, 50_000, Base_Time),
         (Transpose (Base, 3), 50_000, Base_Time + 100),
         (Transpose (Base, 7), 50_000, Base_Time + 200),
         (Transpose (Base, 14), 50_000, Base_Time + 400));

      Synth : constant Poly :=
        Create_Polyphonic (8, Create_Voice'Unrestricted_Access)
        .Add_Notes (Chord ((C, 2), 1))
        .Add_Notes (Chord ((F, 2), 50_000))
        .Add_Notes (Chord ((C, 3), 100_000));

      Main : constant Generator_Access :=
        Create_Mixer ((
                      0 => (Synth, 0.45)
                     ));
   end Poly_Synth;

end ASL_Examples;
