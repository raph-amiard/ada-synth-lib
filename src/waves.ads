with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Waves is

   subtype Period_Buffer is Generator_Buffer;

   type Wave_Generator is abstract new Generator with record
      Frequency_Provider : Generator_Access := null;
   end record;

   procedure Update_Period
     (Self : in out Wave_Generator'Class; Buffer : in out Period_Buffer);

   overriding function Children
     (Self : in out Wave_Generator) return Generator_Array
   is (0 => Self.Frequency_Provider);

   -------------------
   -- Saw_Generator --
   -------------------

   type Saw_Generator is new Wave_Generator with record
      Current : Sample := -1.0;
      Step    : Float := 0.0;
   end record;

   function Create_Saw
     (Freq_Provider : Generator_Access) return access Saw_Generator;
   overriding procedure Next_Samples
     (Self : in out Saw_Generator; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Saw_Generator);

   ----------------------
   -- Square_Generator --
   ----------------------

   type Square_Generator is new Wave_Generator with record
      Is_High        : Boolean;
      Current_Sample : Sample_Period := 0;
   end record;

   function Create_Square
     (Freq_Provider : access Generator'Class) return access Square_Generator;
   overriding procedure Next_Samples
     (Self : in out Square_Generator; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Square_Generator);

   --------------------
   -- Sine_Generator --
   --------------------

   type Sine_Generator is new Wave_Generator with record
      Current_Sample : Sample_Period;
      Current_P      : Period;
   end record;

   function Create_Sine
     (Freq_Provider : access Generator'Class) return access Sine_Generator;
   overriding procedure Next_Samples
     (Self : in out Sine_Generator; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Sine_Generator);

   ---------------------
   -- Noise Generator --
   ---------------------

   type Noise_Generator is new Wave_Generator with null record;

   function Create_Noise return access Noise_Generator;

   overriding procedure Next_Samples
     (Self : in out Noise_Generator; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Noise_Generator);

   ---------------
   -- Pitch Gen --
   ---------------

   type Pitch_Gen is new Generator with record
      Current_Note   : Note_T := (C, 4);
      Current_Freq   : Frequency;
      Relative_Pitch : Integer;
      Source         : access Note_Generator'Class;
      Proc           : Generator_Access := null;
   end record;

   function Create_Pitch_Gen
     (Rel_Pitch : Integer;
      Source : access Note_Generator'Class; Proc : Generator_Access := null)
      return access Pitch_Gen
   is
     (new Pitch_Gen'(Relative_Pitch => Rel_Pitch, Source => Source,
                     Proc => Proc,
                     Current_Freq => Note_To_Freq ((A, 4), Rel_Pitch),
                     others => <>));

   overriding procedure Next_Samples
     (Self : in out Pitch_Gen; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Pitch_Gen);

   overriding function Children
     (Self : in out Pitch_Gen) return Generator_Array
   is (0 => Self.Proc);

   ---------------
   -- Fixed_Gen --
   ---------------

   type Fixed_Gen is new Generator with record
      Val         : Sample;
      Proc        : Generator_Access := null;
      Name        : Unbounded_String;
      Min         : Float := 0.0;
      Max         : Float := 5_000.0;
      Param_Scale : Param_Scale_T := Linear;
   end record;

   type Fixed_Generator is access all Fixed_Gen;

   function Fixed
     (Freq        : Frequency;
      Modulator   : Generator_Access := null;
      Name        : String := "";
      Min         : Float := 0.0;
      Max         : Float := 5_000.0;
      Param_Scale : Param_Scale_T := Linear)
      return access Fixed_Gen;

   overriding procedure Next_Samples
     (Self : in out Fixed_Gen; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Fixed_Gen);

   overriding function Children
     (Self : in out Fixed_Gen) return Generator_Array
   is (0 => Self.Proc);

   overriding function Is_Param (Self : in out Fixed_Gen) return Boolean
   is (True);

   overriding function Nb_Values
     (Self : in out Fixed_Gen) return Natural is (1);

   overriding procedure Set_Value
     (Self : in out Fixed_Gen; I : Natural; Val : Float);

   overriding function Get_Value
     (Self : in out Fixed_Gen; Dummy : Natural) return Float
   is (Float (Self.Val));

   overriding function Get_Name
     (Self : in out Fixed_Gen; Dummy : Natural) return String
   is
     (To_String (Self.Name));

   overriding function Get_Min_Value
     (Self : in out Fixed_Gen; Dummy : Natural) return Float is (Self.Min);

   overriding function Get_Max_Value
     (Self : in out Fixed_Gen; Dummy : Natural) return Float is (Self.Max);

   overriding function Get_Scale
     (Self : in out Fixed_Gen; Dummy : Natural) return Param_Scale_T
   is
      (Self.Param_Scale);

   -----------
   -- Chain --
   -----------

   type Signal_Processors is
     array (Natural range <>) of Signal_Processor_Access;
   No_Signal_Processors : Signal_Processors (1 .. 0) := (others => <>);

   type Signal_Processor_Vector is
     array (Natural range 0 .. 1024) of Signal_Processor_Access;

   type Chain is new Generator with record
      Gen           : Generator_Access;
      Processors    : Signal_Processor_Vector;
      Nb_Processors : Natural := 0;
   end record;

   function Create_Chain
     (Gen : access Generator'Class;
      Sig_Procs : Signal_Processors
         := No_Signal_Processors) return access Chain;

   procedure Add_Processor
     (Self : in out Chain; P : Signal_Processor_Access);

   overriding procedure Next_Samples
     (Self : in out Chain; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out Chain);

   overriding function Children
     (Self : in out Chain) return Generator_Array
   is (Empty_Generator_Array);

   ---------
   -- LFO --
   ---------

   function LFO (Freq : Frequency; Amplitude : Float) return Generator_Access;

   ----------
   -- ADSR --
   ----------

   type ADSR_State is (Running, Release, Off);
   type ADSR is new Generator with record
      Source                 : access Note_Generator'Class;
      Attack, Decay, Release : Sample_Period;
      Sustain                : Scale;
      Cur_Sustain            : Scale;
      Current_P              : Sample_Period;
      State                  : ADSR_State;
      Memo_Sample            : Sample := 0.0;
   end record;

   function Create_ADSR
     (Attack, Decay, Release : Millisecond;
      Sustain : Scale;
      Source : access Note_Generator'Class := null) return access ADSR;

   overriding procedure Next_Samples
     (Self : in out ADSR; Buffer : in out Generator_Buffer);
   overriding procedure Reset (Self : in out ADSR);

   overriding function Children
     (Self : in out ADSR) return Generator_Array
   is (Empty_Generator_Array);

   overriding function Is_Param (Self : in out ADSR) return Boolean
   is (True);

   overriding function Nb_Values
     (Self : in out ADSR) return Natural is (4);

   overriding procedure Set_Value
     (Self : in out ADSR; I : Natural; Val : Float);

   overriding function Get_Value
     (Self : in out ADSR; I : Natural) return Float
   is
     (case I is
         when 0 => Period_To_Sec (Self.Attack),
         when 1 => Period_To_Sec (Self.Decay),
         when 2 => Float (Self.Sustain),
         when 3 => Period_To_Sec (Self.Release),
         when others => raise Constraint_Error);

   overriding function Get_Name
     (Self : in out ADSR; I : Natural) return String
   is
     (case I is
         when 0      => "Attack",
         when 1      => "Decay",
         when 2      => "Sustain",
         when 3      => "Release",
         when others => raise Constraint_Error);

   overriding function Get_Min_Value
     (Self : in out ADSR; I : Natural) return Float is
     (case I is
         when 0      => 0.0,
         when 1      => 0.0,
         when 2      => 0.0,
         when 3      => 0.0,
         when others => raise Constraint_Error);

   overriding function Get_Max_Value
     (Self : in out ADSR; I : Natural) return Float is
     (case I is
         when 0      => 1.0,
         when 1      => 3.0,
         when 2      => 1.0,
         when 3      => 10.0,
         when others => raise Constraint_Error);

   overriding function Get_Scale
     (Self : in out ADSR; I : Natural) return Param_Scale_T is
     (case I is
      when 0 | 1 | 3 => Exp, when others => Linear);

end Waves;
