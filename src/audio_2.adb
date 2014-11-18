with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Utils; use Utils;
with Waves; use Waves;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Command; use Command;
pragma Warnings (Off);
with BLIT; use BLIT;
pragma Warnings (On);

procedure Audio_2 is

   Int_Smp : Short_Integer := 0;
   function Sample_To_Uint16 is new Sample_To_Int (Short_Integer);
   Ignore : Integer;
   pragma Unreferenced (Ignore);

   BPM : constant := 120;

   Synth_Seq : constant access Simple_Sequencer :=
     Create_Sequencer (8, BPM, 4);
   Synth_Source : constant Note_Generator_Access :=
     Note_Generator_Access (Synth_Seq);
--     Synth : constant access Sine_Generator := Create_Sine
--       (Create_Pitch_Gen (0, Synth_Source));
   Synth : constant access Low_Pass_Filter :=
     Create_LP
       (Create_Mixer
          ((
           3 => (BLIT.Create_Saw
                 (Create_Pitch_Gen
                    (-24, Synth_Source, LFO (8.0, 10.0))), 0.5),
           2 => (BLIT.Create_Saw
                 (Create_Pitch_Gen
                    (-12, Synth_Source, LFO (8.0, 10.0))), 0.5),
           1 => (BLIT.Create_Saw
                 (Create_Pitch_Gen
                    (-17, Synth_Source, LFO (8.0, 10.0))), 0.5)
          )),
        Fixed (200.0,
          Proc => new Attenuator'
            (Level => 15500.0,
             Source => Create_ADSR (200, 150, 1000, 0.005, Synth_Source),
             others => <>)),
        0.6);

   Main_Mixer : constant access Mixer :=
     Create_Mixer ((
                   1 => (Synth, 0.5)
                  ));

   SNL : constant Sample_Period := 4000;
   Oct : constant Octave_T := 4;

   S1 : constant Sequencer_Note := ((C, Oct), SNL);
   S2 : constant Sequencer_Note := ((F, Oct), SNL);
   S3 : constant Sequencer_Note := ((D_Sh, Oct), SNL);
   S4 : constant Sequencer_Note := ((A_Sh, Oct), SNL);
   S5 : constant Sequencer_Note := ((G, Oct), SNL);
   S6 : constant Sequencer_Note := ((D_Sh, Oct), SNL);
begin

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

end Audio_2;
