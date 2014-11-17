with Ada.Numerics; use Ada.Numerics;
with Effects; use Effects;

package body Waves is

   -------------------
   -- Update_Period --
   -------------------

   procedure Update_Period (Self : in out Wave_Generator'Class) is
   begin
      Self.P :=
        Utils.Period_In_Samples
          (Frequency (Self.Frequency_Provider.Next_Sample));
   end Update_Period;

   ------------
   -- Create --
   ------------

   function Create_Saw
     (Freq_Provider : Generator_Access) return access Saw_Generator
   is
   begin
      return new Saw_Generator'(Frequency_Provider => Freq_Provider,
                                Current => -1.0, others => <>);
   end Create_Saw;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Saw_Generator) return Sample is
   begin
      Update_Period (Self);
      Self.Step := 2.0 / Float (Self.P);
      Self.Current := Self.Current + Sample (Self.Step);
      if Self.Current > 1.0 then
         Self.Current := Self.Current - 2.0;
      end if;
      return Self.Current;
   end Next_Sample;

   ------------
   -- Create --
   ------------

   function Create_Square
     (Freq_Provider : Generator_Access) return access Square_Generator is
   begin
      return new Square_Generator'(Frequency_Provider => Freq_Provider,
                                   Is_High => True,
                                   Current_Sample => 0,
                                   others => <>);
   end Create_Square;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Square_Generator) return Sample
   is
   begin
      Update_Period (Self);
      Self.Current_Sample := Self.Current_Sample + 1;
      declare
         A : constant Period := Period (Self.Current_Sample) / Self.P;
      begin
         if A >= 1.0 then
            Self.Current_Sample := 0;
            return 1.0;
         end if;
         return (if A >= 0.5 then 1.0 else -1.0);
      end;
   end Next_Sample;

   ------------
   -- Create --
   ------------

   function Create_Sine
     (Freq_Provider : Generator_Access) return access Sine_Generator
   is
      Ret : constant access Sine_Generator :=
        new Sine_Generator'(Frequency_Provider => Freq_Provider,
                            Current_Sample => 0,
                            Current_P => 0.0,
                            others => <>);
   begin
      Update_Period (Ret.all);
      Ret.Current_P := Ret.P;
      return Ret;
   end Create_Sine;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Sine_Generator) return Sample is
   begin
      Update_Period (Self);
      Self.P := Self.P * 2.0;
      Self.Current_Sample := Self.Current_Sample + 1;
      if Period (Self.Current_Sample) >= Self.Current_P then
         Self.Current_P := Self.P;
         Self.Current_Sample := 0;
      end if;
      return Sample
        (Sin
          (Float (Self.Current_Sample) / Float (Self.Current_P) * Pi * 2.0));
   end Next_Sample;

   ------------
   -- Create --
   ------------

   function Create_Chain
     (Gen : access Generator'Class;
      Sig_Procs : Signal_Processors
        := No_Signal_Processors) return access Chain
   is
      Ret : constant access Chain :=
        new Chain'(Gen => Generator_Access (Gen), others => <>);
   begin
      for P of Sig_Procs loop
         Ret.Add_Processor (P);
      end loop;
      return Ret;
   end Create_Chain;

   -------------------
   -- Add_Processor --
   -------------------

   procedure Add_Processor
     (Self : in out Chain; P : Signal_Processor_Access) is
   begin
      Self.Processors (Self.Nb_Processors) := P;
      Self.Nb_Processors := Self.Nb_Processors + 1;
   end Add_Processor;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Chain) return Sample
   is
      S : Sample := Self.Gen.Next_Sample;
   begin
      for I in 0 .. Self.Nb_Processors - 1 loop
         S := Self.Processors (I).Process (S);
      end loop;
      return S;
   end Next_Sample;

   ---------
   -- LFO --
   ---------

   function LFO (Freq : Frequency; Amplitude : Float) return Generator_Access
   is
      Sin : constant Generator_Access := Create_Sine (Fixed (Freq));
   begin
      return new Attenuator'
        (Level => Amplitude,
         Source => new Transposer'(Source => Sin, others => <>), others => <>);
   end LFO;

   ------------
   -- Create --
   ------------

   function Create_ADSR
     (Attack, Decay, Release : Millisecond; Sustain : Scale;
      Source : Note_Generator_Access := null) return access ADSR
   is
   begin
      return new ADSR'(State     => Off,
                       Source    => Source,
                       Attack    => Msec_To_Period (Attack),
                       Decay     => Msec_To_Period (Decay),
                       Release   => Msec_To_Period (Release),
                       Sustain   => Sustain,
                       Current_P => 0, others => <>);
   end Create_ADSR;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample (Self : in out ADSR) return Sample
   is
      Ret : Sample;
   begin
      case Self.Source.Memo_Signal.Kind is
         when On =>
            Self.Current_P := 0;
            Self.State := Running;
         when Off =>
            Self.State := Release;
            Self.Cur_Sustain := Scale (Self.Memo_Sample);
            Self.Current_P := 0;
         when No_Signal => null;
      end case;

      Self.Current_P := Self.Current_P + 1;

      case Self.State is
         when Running =>
            if Self.Current_P in 0 .. Self.Attack then
               Ret := Sample (Self.Current_P) / Sample (Self.Attack);
            elsif
              Self.Current_P in Self.Attack + 1 .. Self.Attack + Self.Decay
            then
               Ret :=
                 (Sample ((Self.Decay - (Self.Current_P - Self.Attack)))
                  / Sample (Self.Decay)
                  * Sample (1.0 - Self.Sustain))
                   + Sample (Self.Sustain);
            else
               Ret := Sample (Self.Sustain);
            end if;
            Self.Memo_Sample := Ret;
         when Release =>
            if Self.Current_P in 0 .. Self.Release then
               Ret :=
                 Sample (Self.Release - Self.Current_P) / Sample (Self.Release)
                   * Sample (Self.Cur_Sustain);
            else
               Self.State := Off;
               Ret := 0.0;
            end if;
         when Off  => Ret := 0.0;
      end case;
      return Ret;
   end Next_Sample;

   ----------------------
   -- Next_Sample --
   ----------------------

   overriding function Next_Sample
     (Self : in out Pitch_Gen) return Sample
   is
      Ret : Sample;
   begin
      case Self.Source.Memo_Signal.Kind is
         when On =>
            Self.Current_Note := Self.Source.Memo_Signal.Note;
            Self.Current_Freq :=
              Note_To_Freq (Self.Current_Note, Self.Relative_Pitch);
         when others => null;
      end case;

      Ret := Sample (Self.Current_Freq);

      if Self.Proc /= null then
         Ret := Ret + Self.Proc.Next_Sample;
      end if;

      return Ret;
   end Next_Sample;

   ------------------
   -- Create_Noise --
   ------------------

   function Create_Noise return access Noise_Generator
   is
      N : constant access Noise_Generator := new Noise_Generator;
   begin
      N.Gen := new GNAT.Random_Numbers.Generator;
      GNAT.Random_Numbers.Reset (N.Gen.all);
      return N;
   end Create_Noise;

   ----------------------
   -- Next_Sample --
   ----------------------

   overriding function Next_Sample
     (Self : in out Noise_Generator) return Sample
   is
   begin
      return Sample (Float'(GNAT.Random_Numbers.Random (Self.Gen.all)));
   end Next_Sample;

end Waves;
