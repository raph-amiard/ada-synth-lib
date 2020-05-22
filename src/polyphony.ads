with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;
with Ada.Containers.Ordered_Multisets;
with Command; use Command;

package Polyphony is

   type Polyphonic_Instrument (Nb_Voices : Positive)
   is new Generator with private;

   type Poly is access all Polyphonic_Instrument'Class;

   type Voice_Constructor
   is access function
     (Note_Gen : Note_Generator_Access) return Generator_Access;

   function Create_Polyphonic
     (Nb_Voices : Natural; Constructor : Voice_Constructor) return Poly;

   function Add_Note
     (Self : access Polyphonic_Instrument; N : Note) return Poly;

private
   function "<" (L, R : Note) return Boolean is
     (L.Time < R.Time);

   package Notes_Sets
   is new Ada.Containers.Ordered_Multisets (Note);

   type Command_Array
   is array (Positive range <>) of aliased Simple_Command;

   type Polyphonic_Instrument (Nb_Voices : Positive)
   is new Generator with record
      Notes            : Notes_Sets.Set;
      Notes_Generators : Command_Array (1 .. Nb_Voices);
      Mixer            : Generator_Access;
      Current_Voice    : Natural := 0;
   end record;

   overriding procedure Next_Samples
     (Self : in out Polyphonic_Instrument; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Polyphonic_Instrument);

   overriding function Children
     (Self : in out Polyphonic_Instrument) return Generator_Array
   is (Empty_Generator_Array);

end Polyphony;
