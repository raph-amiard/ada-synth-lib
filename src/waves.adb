with Effects; use Effects;
with Interfaces; use Interfaces;

package body Waves is

   function Mod_To_Int (A : Unsigned_32) return Integer_32;

   -------------------
   -- Update_Period --
   -------------------

   procedure Update_Period (Self : in out Wave_Generator'Class) is
   begin
      Self.Frequency_Provider.Next_Samples;
      for I in B_Range_T'Range loop
         Self.P_Buffer (I) :=
           Utils.Period_In_Samples
             (Frequency (Self.Frequency_Provider.Buffer (I)));
      end loop;
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

   overriding procedure Next_Samples
     (Self : in out Saw_Generator) is
   begin
      Update_Period (Self);
      for I in B_Range_T'Range loop
         Self.Step := 2.0 / Float (Self.P_Buffer (I));
         Self.Current := Self.Current + Sample (Self.Step);
         if Self.Current > 1.0 then
            Self.Current := Self.Current - 2.0;
         end if;
         Self.Buffer (I) := Self.Current;
      end loop;
   end Next_Samples;

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

   overriding procedure Next_Samples
     (Self : in out Square_Generator)
   is
   begin
      Update_Period (Self);
      for I in B_Range_T'Range loop
         Self.Current_Sample := Self.Current_Sample + 1;
         declare
            A : constant Period := Period (Self.Current_Sample)
              / Self.P_Buffer (I);
         begin
            if A >= 1.0 then
               Self.Current_Sample := 0;
               Self.Buffer (I) := 1.0;
            end if;
            Self.Buffer (I) := (if A >= 0.5 then 1.0 else -1.0);
         end;
      end loop;
   end Next_Samples;

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
      Ret.Current_P := 0.0;
      return Ret;
   end Create_Sine;

   -----------------
   -- Next_Sample --
   -----------------

   overriding procedure Next_Samples
     (Self : in out Sine_Generator) is
   begin
      Update_Period (Self);
      for I in B_Range_T'Range loop
         Self.Current_Sample := Self.Current_Sample + 1;
         if Period (Self.Current_Sample) >= Self.Current_P then
            Self.Current_P := Self.P_Buffer (I) * 2.0;
            Self.Current_Sample := 0;
         end if;
         Self.Buffer (I) :=
           Sample
             (Sin
                (Float (Self.Current_Sample)
                 / Float (Self.Current_P) * Pi * 2.0));
      end loop;
   end Next_Samples;

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

   overriding procedure Next_Samples
     (Self : in out Chain)
   is
      S : Sample;
   begin
      Self.Gen.Next_Samples;
      for J in B_Range_T'Range loop
         S := Self.Gen.Buffer (J);
         for I in 0 .. Self.Nb_Processors - 1 loop
            S := Self.Processors (I).Process (S);
         end loop;
         Self.Buffer (J) := S;
      end loop;
   end Next_Samples;

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

   overriding procedure Next_Samples (Self : in out ADSR)
   is
      Ret : Sample;
   begin
      for I in B_Range_T'Range loop
         case Self.Source.Buffer (I).Kind is
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

         Self.Buffer (I) := Ret;
      end loop;
   end Next_Samples;

   ----------------------
   -- Next_Sample --
   ----------------------

   overriding procedure Next_Samples
     (Self : in out Pitch_Gen)
   is
      Ret : Sample;
   begin
      if Self.Proc /= null then
         Self.Proc.Next_Samples;
      end if;

      for I in B_Range_T'Range loop
         case Self.Source.Buffer (I).Kind is
         when On =>
            Self.Current_Note := Self.Source.Buffer (I).Note;
            Self.Current_Freq :=
              Note_To_Freq (Self.Current_Note, Self.Relative_Pitch);
         when others => null;
         end case;

         Ret := Sample (Self.Current_Freq);

         if Self.Proc /= null then
            Ret := Ret + Self.Proc.Buffer (I);
         end if;

         Self.Buffer (I) := Ret;
      end loop;
   end Next_Samples;

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

   F_Level : constant Sample := 2.0 / Sample (16#FFFFFFFF#);
   G_X1 : Unsigned_32 := 16#67452301#;
   G_X2 : Unsigned_32 := 16#EFCDAB89#;
   Z : constant := 2 ** 31;

   ----------------
   -- Mod_To_Int --
   ----------------

   function Mod_To_Int (A : Unsigned_32) return Integer_32 is
      Res : Integer_32;
   begin
      if A < Z then
         return Integer_32 (A);
      else
         Res := Integer_32 (A - Z);
         Res := Res - (Z - 1) - 1;
         return Res;
      end if;
   end Mod_To_Int;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Noise_Generator)
   is
   begin
      for I in B_Range_T'Range loop
         G_X1 := G_X1 xor G_X2;
         Self.Buffer (I) := Sample (Mod_To_Int (G_X2)) * F_Level;
         G_X2 := G_X2 + G_X1;
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Fixed_Gen) is
   begin
      if Self.Proc /= null then
         Self.Proc.Next_Samples;
      end if;

      for I in B_Range_T'Range loop
         Self.Buffer (I) := Self.Val;
         if Self.Proc /= null then
            Self.Buffer (I) := Self.Val + Self.Proc.Buffer (I);
         end if;
      end loop;
   end Next_Samples;

end Waves;
