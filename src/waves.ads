with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with GNAT.Random_Numbers;

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

   function Create_Saw
     (Freq_Provider : Generator_Access) return access Saw_Generator;
   overriding function Next_Sample_Impl
     (Self : in out Saw_Generator) return Sample;

   ----------------------
   -- Square_Generator --
   ----------------------

   type Square_Generator is new Wave_Generator with record
      Is_High : Boolean;
      Current_Sample : Period;
   end record;

   function Create_Square
     (Freq_Provider : Generator_Access) return access Square_Generator;
   overriding function Next_Sample_Impl
     (Self : in out Square_Generator) return Sample;

   --------------------
   -- Sine_Generator --
   --------------------

   type Sine_Generator is new Wave_Generator with record
      Current_Sample : Period;
      Current_P : Period;
   end record;

   function Create_Sine
     (Freq_Provider : Generator_Access) return access Sine_Generator;
   overriding function Next_Sample_Impl
     (Self : in out Sine_Generator) return Sample;

   ---------------------
   -- Noise Generator --
   ---------------------

   type Noise_Generator is new Wave_Generator with record
      Gen : access GNAT.Random_Numbers.Generator;
   end record;

   function Create_Noise return access Noise_Generator;

   overriding function Next_Sample_Impl
     (Self : in out Noise_Generator) return Sample;

   ---------------
   -- Pitch Gen --
   ---------------

   type Pitch_Gen is new Generator with record
      Current_Note : Note_T := (C, 4);
      Relative_Pitch : Integer;
      Source : Note_Generator_Access;
      Proc : Generator_Access := null;
   end record;

   function Create_Pitch_Gen
     (Rel_Pitch : Integer;
      Source : Note_Generator_Access; Proc : Generator_Access := null)
      return access Pitch_Gen
   is
     (new Pitch_Gen'(Relative_Pitch => Rel_Pitch, Source => Source,
                     Proc => Proc, others => <>));

   overriding function Next_Sample_Impl
     (Self : in out Pitch_Gen) return Sample;

   ---------------
   -- Fixed_Gen --
   ---------------

   type Fixed_Gen is new Generator with record
      Val : Sample;
      Proc : Generator_Access := null;
   end record;

   function Fixed
     (F : Frequency; Proc : Generator_Access := null) return access Fixed_Gen
   is
     (new Fixed_Gen'(Val => Sample (F), Proc => Proc, others => <>));

   overriding function Next_Sample_Impl
     (Self : in out Fixed_Gen) return Sample
   is
     (if Self.Proc /= null
      then Self.Val + Self.Proc.Next_Sample
      else Self.Val);

   -----------
   -- Chain --
   -----------

   type Signal_Processors is
     array (Natural range <>) of Signal_Processor_Access;
   No_Signal_Processors : Signal_Processors (1 .. 0) := (others => <>);

   type Signal_Processor_Vector is
     array (Natural range 0 .. 1024) of Signal_Processor_Access;

   type Chain is new Generator with record
      Gen : Generator_Access;
      Processors : Signal_Processor_Vector;
      Nb_Processors : Natural := 0;
   end record;

   function Create_Chain
     (Gen : access Generator'Class;
      Sig_Procs : Signal_Processors
         := No_Signal_Processors) return access Chain;

   procedure Add_Processor
     (Self : in out Chain; P : Signal_Processor_Access);

   overriding function Next_Sample_Impl
     (Self : in out Chain) return Sample;

   ---------
   -- LFO --
   ---------

   function LFO (Freq : Frequency; Amplitude : Float) return Generator_Access;

   ----------
   -- ADSR --
   ----------

   type ADSR_State is (Running, Release, Off);
   type ADSR is new Generator with record
      Source : Note_Generator_Access;
      Attack, Decay, Release : Period;
      Sustain : Scale;
      Cur_Sustain : Scale;
      Current_P : Period;
      State : ADSR_State;
   end record;

   function Create_ADSR
     (Attack, Decay, Release : Millisecond;
      Sustain : Scale;
      Source : Note_Generator_Access := null) return access ADSR;

   overriding function Next_Sample_Impl (Self : in out ADSR) return Sample;
end Waves;
