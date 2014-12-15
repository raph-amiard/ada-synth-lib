with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;

package Command is
   type Simple_Command is new Note_Generator with record
      On_Period, Off_Period, Current_P : Sample_Period;
      Note : Note_T;
   end record;

   function Create_Simple_Command
     (On_Period, Off_Period : Sample_Period;
      Note : Note_T) return Note_Generator_Access;

   overriding procedure Next_Messages
     (Self : in out Simple_Command);

   type Sequencer_Note is record
      Note : Note_T;
      Duration : Sample_Period;
   end record;

   No_Seq_Note : Sequencer_Note := (Note => No_Note, Duration => 0);
   type Notes_Array is array (Natural range <>) of Sequencer_Note;
   No_Notes : Notes_Array (1 .. 0) := (others => <>);

   type Simple_Sequencer (Nb_Steps : Natural) is new Note_Generator with record
      BPM : Natural := 120;
      Notes : Notes_Array (1 .. Nb_Steps) := (others => No_Seq_Note);
      Interval : Sample_Period := 0;
      Current_Note : Natural := 0;
   end record;

   function Create_Sequencer
     (Nb_Steps, BPM : Natural;
      Measures : Natural := 1;
      Notes : Notes_Array := No_Notes) return access Simple_Sequencer;

   overriding procedure Next_Messages
     (Self : in out Simple_Sequencer);

end Command;
