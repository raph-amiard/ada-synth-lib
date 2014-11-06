with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;

package Command is
   type Simple_Command is new Note_Generator with record
      On_Period, Off_Period, Current_P : Period;
      Note : Note_T;
   end record;

   function Create_Simple_Command
     (On_Period, Off_Period : Period;
      Note : Note_T) return Note_Generator_Access;

   overriding function Next_Message_Impl
     (Self : in out Simple_Command) return Note_Signal;

   type Sequencer_Note is record
      Note : Note_T;
      Duration : Period;
   end  record;

   No_Seq_Note : Sequencer_Note := (Note => No_Note, Duration => 0);
   type Notes_Array is array (Natural range <>) of Sequencer_Note;

   type Simple_Sequencer (Nb_Steps : Natural) is new Note_Generator with record
      BPM : Natural := 120;
      Notes : Notes_Array (1 .. Nb_Steps) := (others => No_Seq_Note);
      Interval, Current_P : Period := 0;
      Current_Note : Natural := 0;
   end record;

   function Create_Sequencer
     (Nb_Steps, BPM : Natural;
      Measures : Natural := 1) return access Simple_Sequencer;

   overriding function Next_Message_Impl
     (Self : in out Simple_Sequencer) return Note_Signal;

end Command;
