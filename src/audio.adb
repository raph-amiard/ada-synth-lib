with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Utils; use Utils;
with Waves; use Waves;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Command; use Command;
pragma Warnings (Off);
with BLIT; use BLIT;

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
                   (Level => 300.0,
                    Source => Create_ADSR (0, 50, 10000, 0.1, Kick_Source),
                    others => <>))),

              0.7)
       ), Env => Create_ADSR (10, 1000, 200, 0.2, Kick_Source));

   Snare_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (16, BPM, 4);
   Snare_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Snare_Seq);
   Snare : constant access Mixer :=
     Create_Mixer
          ((
           1 => (Create_Noise, 0.5),
           2 => (Create_Sine
                 (Create_Pitch_Gen
                    (12, Snare_Source,
                       Proc =>
                          new Attenuator'
                         (Level => 300.0,
                          Source =>
                            Create_ADSR (0, 200, 10000, 0.5, Snare_Source),
                          others => <>))),
                 0.1)
          ), Env => Create_ADSR (0, 100, 100, 0.2, Snare_Source));

   Hat_Seq : constant access Simple_Sequencer := Create_Sequencer (16, BPM);
   Hat_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Hat_Seq);
   Hat : constant access Mixer :=
     Create_Mixer
       ((
        1 => (Create_Noise, 0.5)
       ), Env => Create_ADSR (0, 20, 0, 0.0, Hat_Source));

   Synth_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (8, BPM, 4);
   Synth_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq);

   Synth : constant access Disto :=
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
                         (-17, Synth_Source)), 0.5)
             )),
           Fixed (200.0,
             Proc => new Attenuator'
               (Level => 500.0,
                Source => Create_ADSR (10, 150, 200, 0.005, Synth_Source),
                others => <>)),
           0.1), 0.00001, 1.5);

   Main_Mixer : constant access Mixer :=
     Create_Mixer ((
                   (Kick, 0.5),
                   (Snare, 0.7),
                   (Hat, 0.6),
                   (Synth, 0.45)
                  ));

   o : constant Sequencer_Note := No_Seq_Note;
   K : constant Sequencer_Note := ((G, 3), 3000);
   Z : constant Sequencer_Note := ((G, 3), 5000);
   B : constant Sequencer_Note := ((G, 3), 8000);

   SNL : constant Sample_Period := 4000;

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
      Next_Steps;
      Main_Mixer.Next_Samples;
      for I in B_Range_T'Range loop
         Int_Smp := Sample_To_Uint16 (Main_Mixer.Buffer (I));
         Ignore := GNAT.OS_Lib.Write
           (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
      end loop;
      exit when Sample_Nb > 10_000_000;
      Sample_Nb := Sample_Nb + Generator_Buffer_Length;
   end loop;

end Audio;
