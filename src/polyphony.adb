with Effects; use Effects;
with Ada.Text_IO; use Ada.Text_IO;

package body Polyphony is

   -----------------------
   -- Create_Polyphonic --
   -----------------------

   function Create_Polyphonic
     (Nb_Voices : Natural; Constructor : Voice_Constructor) return Poly
   is
      Ret : Poly := new Polyphonic_Instrument (Nb_Voices);
      Voices : Generators_Arg_Array (0 .. Nb_Voices - 1);
   begin
      for J in 0 .. Nb_Voices - 1 loop
         Voices (J) :=
           (Constructor (Ret.Notes_Generators (J + 1)'Access), 1.0);
      end loop;
      Ret.Mixer := Create_Mixer (Voices);
      return Ret;
   end Create_Polyphonic;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Polyphonic_Instrument; Buffer : in out Generator_Buffer)
   is
      use Notes_Sets;
      Note_Cursor : Cursor := Self.Notes.Ceiling ((No_Note, 0, Sample_Nb));
      Last_Sample : constant Sample_Period :=
        Sample_Nb + Sample_Period (Buffer_Range_Type'Last);
      N : Note;
   begin
      --  While we have notes, assign them to voices
      while Has_Element (Note_Cursor) loop
         N := Element (Note_Cursor);
         Put_Line ("HAS NOTE ! " & N.Time'Image & " " & N.Duration'Image);

         exit when N.Time > Last_Sample;
         Self.Notes_Generators (Self.Current_Voice + 1) := Simple_Command'
           (On_Period  => N.Time,
            Off_Period => N.Time + N.Duration,
            Note       => N.Note,
            Current_P  => 0,
            Buffer     => <>);

         Self.Current_Voice :=
           (Self.Current_Voice + 1) mod Self.Nb_Voices;

         Next (Note_Cursor);
      end loop;

      --  Trigger notes for simple commands
      for N of Self.Notes_Generators loop
         N.Next_Step;
      end loop;

      --  Then generate sound from mixer containing subvoices.
      Self.Mixer.Next_Samples (Buffer);
   end Next_Samples;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Polyphonic_Instrument) is
   begin
      Self.Mixer.Reset;
      Self.Notes.Clear;
      for C of Self.Notes_Generators loop
         C.Reset;
      end loop;
   end Reset;

   --------------
   -- Add_Note --
   --------------

   function Add_Note
     (Self : access Polyphonic_Instrument; N : Note) return Poly
   is
   begin
      Put_Line ("Adding note, " & Img (N));
      Self.Notes.Insert (N);
      for El of Self.Notes loop
         Put_Line (Img (El));
      end loop;

      return Poly (Self);
   end Add_Note;

end Polyphony;
