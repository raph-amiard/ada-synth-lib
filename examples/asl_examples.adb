package body ASL_Examples is
   package body Programmatic_Drums is
      procedure Init is
      begin
         for I in -3 .. 0 loop
            Seq
              := Create_Sequencer (16, BPM, 1, Notes);
            Sine_Gen :=
              Simple_Synth (Note_Generator_Access (Seq), I * 12, Decay);
            Mixer.Add_Generator (Sine_Gen, Volume);
            BPM    := BPM * 2;
            Volume := Volume / 1.8;
            Decay  := Decay / 2;
         end loop;
      end Init;
   begin
      Init;
   end Programmatic_Drums;
end ASL_Examples;
