with Command; use Command;
with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Effects; use Effects;
with Waves; use Waves;
with BLIT; use BLIT;
with Polyphony; use Polyphony;

package Poly_Demo is

   BPM : constant := 120;

   SNL : constant Sample_Period := 4000;
   S1 : constant Sequencer_Note := ((C, 3), SNL);
   S2 : constant Sequencer_Note := ((F, 4), SNL);
   S3 : constant Sequencer_Note := ((D_Sh, 4), SNL);
   S4 : constant Sequencer_Note := ((A_Sh, 4), SNL);
   S5 : constant Sequencer_Note := ((G, 4), SNL);
   S6 : constant Sequencer_Note := ((D_Sh, 4), SNL);
   
   function Create_Voice 
     (Note_Gen : Note_Generator_Access) return Generator_Access
   is
     (Generator_Access 
        (Create_LP
             (Create_Mixer
                  ((1 => (BLIT.Create_Saw
                          (Create_Pitch_Gen
                           (0, Note_Gen)), 0.5),
                   2 => (BLIT.Create_Square
                         (Create_Pitch_Gen
                          (0, Note_Gen)), 0.5)
                     
                   ), Volume_Mod => Create_ADSR (100, 1000, 100, 0.2, Note_Gen)),
              Cut_Freq => Fixed (1000.0,
                     Modulator => new Attenuator'
                       (Level => 1500.0,
                        Source => Create_ADSR (10, 150, 200, 0.005, Note_Gen),
                        others => <>)),
              Q => 0.2)));

    
   Synth : constant Poly :=
     Create_Polyphonic (4, Create_Voice'Unrestricted_Access)
     .Add_Note (((C, 2), 100_000, 1))
     .Add_Note (((E, 2), 100_000, 5000))
     .Add_Note (((G, 2), 100_000, 10000))
     .Add_Note (((C, 3), 100_000, 15000));
     
   Main_Mixer : constant access Mixer :=
     Create_Mixer ((
                   0 => (Synth, 0.45)
                  ));

end Poly_Demo;
