with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

package Waves is

   type Wave_Generator is abstract new Generator with record
      Frequency_Provider : Generator_Access := null;
      P : Period;
   end record;

   -------------------
   -- Saw_Generator --
   -------------------

   type Saw_Generator is new Wave_Generator with record
      Current : Sample;
      Step : Float;
   end record;

   function Create (Freq_Provider : Generator_Access) return Saw_Generator;
   overriding function Next_Sample (Self : in out Saw_Generator) return Sample;

   ----------------------
   -- Square_Generator --
   ----------------------

   type Square_Generator is new Wave_Generator with record
      Is_High : Boolean;
      Current_Sample : Period;
   end record;

   function Create (Freq_Provider : Generator_Access) return Square_Generator;
   overriding function Next_Sample
     (Self : in out Square_Generator) return Sample;

   --------------------
   -- Sine_Generator --
   --------------------

   type Sine_Generator is new Wave_Generator with record
      Current_Sample : Period;
      Current_P : Period;
   end record;

   function Create (Freq_Provider : Generator_Access) return Sine_Generator;
   overriding function Next_Sample
     (Self : in out Sine_Generator) return Sample;

   type Signal_Processor_Vector is
     array (Natural range 0 .. 1024) of Signal_Processor_Access;

   ---------------
   -- Fixed_Gen --
   ---------------

   type Fixed_Gen is new Generator with record
      Val : Sample;
      Proc : Generator_Access := null;
   end record;

   function Fixed (F : Frequency; Proc : Generator_Access := null) return Generator_Access is
     (new Fixed_Gen'(Val => Sample (F), Proc => Proc));

   function Next_Sample (Self : in out Fixed_Gen) return Sample is
     (if Self.Proc /= null then Self.Val + Self.Proc.Next_Sample else Self.Val);

   -----------
   -- Chain --
   -----------

   type Chain is new Generator with record
      Gen : access Generator'Class;
      Processors : Signal_Processor_Vector;
      Nb_Processors : Natural := 0;
   end record;

   function Create (Gen : access Generator'Class) return Chain;

   procedure Add_Processor
     (Self : in out Chain; P : Signal_Processor_Access);

   overriding function Next_Sample
     (Self : in out Chain) return Sample;

   function LFO (Freq : Frequency; Amplitude : Float) return Generator_Access;

   subtype Millisecond is Natural;
   type Scale is new Float range 0.0 .. 1.0;

   type ADSR is new Generator with record
      Attack, Decay, Release : Millisecond;
      Sustain : Scale;
   end record;
end Waves;
