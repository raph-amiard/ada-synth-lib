with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

package Effects is

   ----------------
   -- Attenuator --
   ----------------

   type Attenuator is new Signal_Processor with record
      Level : Float;
   end record;

   overriding function Process
     (Self : in out Attenuator; S : Sample) return Sample is
     (S * Sample (Self.Level));

   --------------------
   -- Dyn_Attenuator --
   --------------------

   type Dyn_Attenuator is new Signal_Processor with record
      Level_Provider : Generator_Access;
   end record;

   function VCA (G : Generator_Access) return access Dyn_Attenuator
   is (new Dyn_Attenuator'(Level_Provider => G));

   overriding function Process
     (Self : in out Dyn_Attenuator; S : Sample) return Sample
   is
     (S * Self.Level_Provider.Next_Sample);

   ----------------
   -- Transposer --
   ----------------

   type Transposer is new Signal_Processor with null record;

   overriding function Process
     (Self : in out Transposer; S : Sample) return Sample
   is
     ((S + 1.0) / 2.0);

   -----------
   -- Mixer --
   -----------

   type Mixer_Generator is record
      Gen : access Generator'Class;
      Level : Float;
   end record;

   type Generator_Vector is array (Natural range 0 .. 1024) of Mixer_Generator;

   type Mixer is new Generator with record
      Generators : Generator_Vector;
      Length : Natural := 0;
   end record;

   type Generators_Arg_Array is array (Natural range <>) of Mixer_Generator;
   No_Generators : constant Generators_Arg_Array (1 .. 0) := (others => <>);

   function Create_Mixer
     (Sources : Generators_Arg_Array) return access Mixer;

   function Add_Generator
     (Self : in out Mixer; G : access Generator'Class;
      Level : Float) return Natural;

   procedure Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float);

   overriding function Next_Sample_Impl (Self : in out Mixer) return Sample;

end Effects;
