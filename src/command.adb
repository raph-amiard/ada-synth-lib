with Config; use Config;

package body Command is

   ---------------------------
   -- Create_Simple_Command --
   ---------------------------

   function Create_Simple_Command
     (On_Period, Off_Period : Sample_Period;
      Note : Note_T) return access Simple_Command'Class
   is
   begin
      return N : constant access Simple_Command'Class
        := new Simple_Command'(Note       => Note,
                               Buffer     => <>,
                               On_Period  => On_Period,
                               Off_Period => Off_Period,
                               Current_P  => 0)
      do
         Register_Simulation_Listener (N);
      end return;
   end Create_Simple_Command;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding procedure Next_Step
     (Self : in out Simple_Command)
   is
   begin
      for I in Self.Buffer'Range loop
         Self.Current_P := Self.Current_P + 1;
         if Self.Current_P = Self.On_Period then
            Self.Buffer (I) := Note_Signal'(Kind => On, Note => Self.Note);
         elsif Self.Current_P = Self.Off_Period then
            Self.Buffer (I) := Note_Signal'(Kind => Off, Note => <>);
         else
            Self.Buffer (I) := Note_Signal'(Kind => No_Signal, Note => <>);
         end if;
      end loop;
   end Next_Step;

   ----------------------
   -- Create_Sequencer --
   ----------------------

   function Create_Sequencer
     (Nb_Steps, BPM : Natural;
      Measures      : Natural := 1;
      Notes         : Notes_Array := No_Notes;
      Track_Name    : String := "") return access Simple_Sequencer
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
      Register_Simulation_Listener (Ret);

      Ret.Notes := Notes;
      Ret.Track_Name := To_Unbounded_String (Track_Name);
      return Ret;
   end Create_Sequencer;

   -----------------------
   -- Next_Message_Impl --
   -----------------------

   overriding procedure Next_Step
     (Self : in out Simple_Sequencer)
   is
      Cur_Note  : Sequencer_Note;
      Current_P : Sample_Period;
   begin
      for I in Self.Buffer'Range loop
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
   end Next_Step;

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

   function Note_For_Sample
     (Self : Simple_Sequencer; Sample_Nb : Sample_Period) return Natural
   is
   begin
      return (Natural (Sample_Nb / Self.Interval) mod Self.Nb_Steps) + 1;
   end Note_For_Sample;

   ------------------
   -- To_Seq_Notes --
   ------------------

   function To_Seq_Notes
     (A   : Utils.Scales_Arrays.Array_Type;
      D   : Sample_Period;
      Oct : Octave_T) return Notes_Array
   is
      function N (N : Scale_Degree_T) return Sequencer_Note is (((N, Oct), D));
      function Map is new Utils.Scales_Arrays.Map_Gen
        (Sequencer_Note, Notes_Array, N);
   begin
      return Map (A);
   end To_Seq_Notes;

end Command;
