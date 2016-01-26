with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

package body Command is

   ---------------------------
   -- Create_Simple_Command --
   ---------------------------

   function Create_Simple_Command
     (On_Period, Off_Period : Sample_Period;
      Note : Note_T) return Note_Generator_Access
   is
      N : Note_Generator_Access;
   begin
      N := new Simple_Command'(Note              => Note,
                               Current_Sample_Nb => <>,
                               Buffer            => <>,
                               On_Period         => On_Period,
                               Off_Period        => Off_Period,
                               Current_P         => 0);
      Register_Note_Generator (N);
      return N;
   end Create_Simple_Command;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding procedure Next_Messages
     (Self : in out Simple_Command)
   is
   begin
      for I in B_Range_T'Range loop
         Self.Current_P := Self.Current_P + 1;
         if Self.Current_P = Self.On_Period then
            Self.Buffer (I) := Note_Signal'(Kind => On, Note => Self.Note);
         elsif Self.Current_P = Self.Off_Period then
            Self.Buffer (I) := Note_Signal'(Kind => Off, Note => <>);
         else
            Self.Buffer (I) := Note_Signal'(Kind => No_Signal, Note => <>);
         end if;
      end loop;
   end Next_Messages;

   ----------------------
   -- Create_Sequencer --
   ----------------------

   function Create_Sequencer
     (Nb_Steps, BPM : Natural;
      Measures : Natural := 1;
      Notes : Notes_Array := No_Notes) return access Simple_Sequencer
   is
      Ret : constant access Simple_Sequencer :=
        new Simple_Sequencer'
          (BPM => BPM,
           Nb_Steps => Nb_Steps * Measures,
           Interval => Sample_Period
             (((60.0 / Float (BPM) * 4.0)
              / Float (Nb_Steps)) * Float (SAMPLE_RATE)),
           others => <>);
   begin
      Register_Note_Generator (Note_Generator_Access (Ret));
      Ret.Notes := Notes;
      return Ret;
   end Create_Sequencer;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding procedure Next_Messages
     (Self : in out Simple_Sequencer)
   is
      Cur_Note : Sequencer_Note;
      Current_P : Sample_Period;
   begin
      for I in B_Range_T'Range loop
         Cur_Note :=
           Self.Notes ((Natural
                       ((Sample_Nb + Sample_Period (I)) / Self.Interval)
                       mod Self.Nb_Steps) + 1);
         Current_P := (Sample_Nb + Sample_Period (I)) mod Self.Interval;
         Self.Buffer (I).Kind := No_Signal;
         if Cur_Note.Note /= No_Note then
            if Current_P = Cur_Note.Duration then
               Self.Buffer (I).Kind := Off;
            elsif Current_P = 1 then
               Self.Buffer (I).Kind := On;
               Self.Buffer (I).Note := Cur_Note.Note;
            end if;
         end if;
      end loop;
   end Next_Messages;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Simple_Command) is
   begin
      Self.Current_P := 0;
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Simple_Sequencer) is
   begin
      Self.Current_Note := 0;
   end Reset;

   function Note_For_Sample (Self : Simple_Sequencer; Sample_Nb : Sample_Period) return Natural
   is
   begin
     return (Natural (Sample_Nb / Self.Interval) mod Self.Nb_Steps) + 1;
   end Note_For_Sample;

end Command;
