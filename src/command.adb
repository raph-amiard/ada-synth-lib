with Config; use Config;

package body Command is

   ---------------------------
   -- Create_Simple_Command --
   ---------------------------

   function Create_Simple_Command
     (On_Period, Off_Period : Period;
      Note : Note_T) return Note_Generator_Access
   is
   begin
      return new Simple_Command'(Note              => Note,
                                 Current_Sample_Nb => <>,
                                 Memo_Signal       => <>,
                                 On_Period         => On_Period,
                                 Off_Period        => Off_Period,
                                 Current_P         => 0);
   end Create_Simple_Command;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding function Next_Message_Impl
     (Self : in out Simple_Command) return Note_Signal
   is
   begin
      Self.Current_P := Self.Current_P + 1;
      if Self.Current_P = Self.On_Period then
         return Note_Signal'(Kind => On, Note => Self.Note);
      elsif Self.Current_P = Self.Off_Period then
         return Note_Signal'(Kind => Off, Note => <>);
      else
         return Note_Signal'(Kind => No_Signal, Note => <>);
      end if;
   end Next_Message_Impl;

   ----------------------
   -- Create_Sequencer --
   ----------------------

   function Create_Sequencer
     (Nb_Steps, BPM : Natural;
      Measures : Natural := 1) return access Simple_Sequencer
   is
      Ret : constant access Simple_Sequencer :=
        new Simple_Sequencer'
          (BPM => BPM,
           Nb_Steps => Nb_Steps * Measures,
           Interval => Period ((BPM * Natural (SAMPLE_RATE)) / 60 / Nb_Steps),
           others => <>);
   begin
      return Ret;
   end Create_Sequencer;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding function Next_Message_Impl
     (Self : in out Simple_Sequencer) return Note_Signal
   is
      Cur_Note : constant Sequencer_Note := Self.Notes (Self.Current_Note + 1);
   begin
      Self.Current_P := Self.Current_P + 1;

      if Self.Current_P = 1 and then Cur_Note /= No_Seq_Note then
         return Note_Signal'(Kind => On,
                             Note => Cur_Note.Note);
      elsif Cur_Note /= No_Seq_Note
        and then Self.Current_P = Cur_Note.Duration + 1
      then
         return Note_Signal'(Kind => Off, Note => <>);
      else
         if Self.Current_P >= Self.Interval then
            Self.Current_P := 0;
            Self.Current_Note := (Self.Current_Note + 1) mod Self.Nb_Steps;
         end if;
         return Note_Signal'(Kind => No_Signal, Note => <>);
      end if;

   end Next_Message_Impl;

end Command;
