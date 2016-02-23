with Sound_Gen_Interfaces;        use Sound_Gen_Interfaces;
with Utils;                       use Utils;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Sequencer is

   type Seq_Note is record
      Note     : Note_T;
      Duration : Sample_Period;
      Start    : Sample_Period;
   end record;

   function "<" (L, R : Seq_Note) return Boolean
   is (L.Start'Img & Note_Img (L.Note) < R.Start'Img & Note_Img (R.Note));

   overriding function "=" (L, R : Seq_Note) return Boolean
   is (L.Start = R.Start
       and then L.Note = R.Note);

   No_Seq_Note : constant Seq_Note := (No_Note, 0, 0);

   package Notes_Sets
   is new Ada.Containers.Ordered_Sets (Seq_Note);

   subtype Notes_Set is Notes_Sets.Set;

   type Dummy_Note_Generator is new Note_Generator with null record;
   overriding procedure Reset (Self : in out Dummy_Note_Generator);

   type Note_Generator_Array
   is array (Natural range <>) of access Dummy_Note_Generator;
   type Note_Array is array (Natural range <>) of Seq_Note;

   Max_Polyphony : constant := 16;

   type Sequencer is new I_Simulation_Listener with record
      Notes           : Notes_Set;
      Note_Generators : Note_Generator_Array (0 .. Max_Polyphony);
      Nb_Generators   : Natural;
      Current_Notes   : Note_Array (0 .. Max_Polyphony)
        := (others => No_Seq_Note);
      Next_Channel    : Natural := 0;
      Interval        : Sample_Period;
      BPM             : Natural := 120;
      Track_Name      : Unbounded_String;
   end record;

   function Create
     (BPM        : Natural;
      Measures   : Natural;
      Nb_Voices  : Natural := 8;
      Track_Name : String := "") return access Sequencer;
   procedure Add_Note (Self : in out Sequencer; Note : Seq_Note);
   procedure Remove_Note (Self : in out Sequencer; Note : Seq_Note);
   overriding procedure Next_Step (Self : in out Sequencer);
   overriding function Name (Self : in out Sequencer) return String is
     (To_String (Self.Track_Name));

end Sequencer;
