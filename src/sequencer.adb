with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

package body Sequencer is

   ------------
   -- Create --
   ------------

   function Create
     (BPM        : Natural;
      Measures   : Natural;
      Nb_Voices  : Natural := 8;
      Track_Name : String := "") return access Sequencer
   is
      Ret : constant access Sequencer := new Sequencer'
        (Notes           => <>,
         Current_Notes   => (others => No_Seq_Note),
         Next_Channel    => 0,
         Note_Generators => <>,
         Nb_Generators   => Nb_Voices,
         Interval        =>
           Sample_Period (60.0 / Float (BPM) * 4.0
             * Float (Measures) * Float (SAMPLE_RATE)),
         BPM             => BPM,
         Track_Name      => To_Unbounded_String (Track_Name));
   begin
      for I in 0 .. Nb_Voices - 1 loop
         Ret.Note_Generators (I) := new Dummy_Note_Generator;
      end loop;
      Register_Simulation_Listener (Ret);
      return Ret;
   end Create;

   --------------
   -- Add_Note --
   --------------

   procedure Add_Note (Self : in out Sequencer; Note : Seq_Note) is
   begin
      Self.Notes.Include (Note);
   end Add_Note;

   -----------------
   -- Remove_Note --
   -----------------

   procedure Remove_Note (Self : in out Sequencer; Note : Seq_Note) is
   begin
      Self.Notes.Exclude (Note);
   end Remove_Note;

   ---------------
   -- Next_Step --
   ---------------

   overriding procedure Next_Step (Self : in out Sequencer) is
      use Notes_Sets;
      Current_P : Sample_Period :=
        (Sample_Nb) mod Self.Interval;
      C         : Cursor := Self.Notes.Ceiling
        (Seq_Note'(Start => Current_P, others => <>));
      Note      : Seq_Note;

      procedure Set_Next_Notes;

      procedure Set_Next_Notes is
      begin

         while Has_Element (C) loop
            Note := Element (C);
            exit when Note.Start >= Current_P + Generator_Buffer_Length;
            Self.Current_Notes (Self.Next_Channel) := Note;
            Self.Next_Channel :=
              (Self.Next_Channel + 1) mod Self.Nb_Generators;
            Next (C);
         end loop;
      end Set_Next_Notes;

      Last_P : Sample_Period := Current_P;
   begin
      Set_Next_Notes;

      for J in Buffer_Range_Type'Range loop

         Current_P :=
           (Sample_Nb + Sample_Period (J)) mod Self.Interval;

         if Current_P = 0 and Last_P /= 0 then
            Put_Line ("DAFUQ");
            C := Self.Notes.First;
            Set_Next_Notes;
         end if;

         Last_P := Current_P;

         for I in 0 .. Self.Nb_Generators - 1 loop
            if Self.Current_Notes (I) /= No_Seq_Note then
               Note := Self.Current_Notes (I);
               if Current_P >= (Note.Start + Note.Duration) mod Self.Interval
               then
                  Self.Current_Notes (I) := No_Seq_Note;
                  Self.Note_Generators (I).Buffer (J).Kind := Off;
               else
                  Self.Note_Generators (I).Buffer (J).Kind := On;
                  Self.Note_Generators (I).Buffer (J).Note := Note.Note;
               end if;
            else
               Self.Note_Generators (I).Buffer (J).Kind := No_Signal;
            end if;
         end loop;

      end loop;
   end Next_Step;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Dummy_Note_Generator) is
   begin
      Self.Buffer := (others => (No_Note, Off));
   end Reset;

end Sequencer;
