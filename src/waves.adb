with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Effects; use Effects;
with Ada.Text_IO; use Ada.Text_IO;

package body Waves is

   -------------------
   -- Update_Period --
   -------------------

   procedure Update_Period (Self : in out Wave_Generator'Class) is
   begin
      Self.P :=
        Utils.Period_In_Samples (Frequency (Self.Frequency_Provider.Next_Sample));
   end Update_Period;

   ------------
   -- Create --
   ------------

   function Create (Freq_Provider : Generator_Access) return Saw_Generator
   is
   begin
      return Saw_Generator'(Frequency_Provider => Freq_Provider,
                            Current => -1.0, others => <>);
   end Create;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample (Self : in out Saw_Generator) return Sample is
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

   function Create (Freq_Provider : Generator_Access) return Square_Generator is
   begin
      return Square_Generator'(Frequency_Provider => Freq_Provider,
                               Is_High => True,
                               Current_Sample => 0,
                               others => <>);
   end Create;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Square_Generator) return Sample is
   begin
      Update_Period (Self);
      Self.Current_Sample := Self.Current_Sample + 1;
      if Self.Current_Sample >= Self.P then
         Self.Current_Sample := 0;
         Self.Is_High := not Self.Is_High;
      end if;
      return (if Self.Is_High then 1.0 else -1.0);
   end Next_Sample;

   ------------
   -- Create --
   ------------

   function Create (Freq_Provider : Generator_Access) return Sine_Generator is
   begin
      return Self : Sine_Generator do
         Self := (Frequency_Provider => Freq_Provider,
                  Current_Sample => 0,
                  Current_P => 0,
                  others => <>);
         Update_Period (Self);
         Self.Current_P := Self.P;
      end return;
   end Create;

   -----------------
   -- Next_Sample --
   -----------------

   overriding function Next_Sample
     (Self : in out Sine_Generator) return Sample is
   begin
      Update_Period (Self);
      Self.P := Self.P * 2;
      Self.Current_Sample := Self.Current_Sample + 1;
      if Self.Current_Sample = Self.Current_P then
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

   function Create (Gen : access Generator'Class) return Chain
   is
   begin
      return Chain'(Gen => Gen, others => <>);
   end Create;

   -------------------
   -- Add_Processor --
   -------------------

   procedure Add_Processor
     (Self : in out Chain; P : Signal_Processor_Access) is
   begin
      Put_Line (Standard_Error, Self.Nb_Processors'Img);
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
      Sin : Generator_Access := new Sine_Generator'(Create (Fixed (Freq)));
      LFO_Chain : access Chain := new Chain'(Create (Sin));
   begin
      LFO_Chain.Add_Processor (new Attenuator'(Level => Amplitude));
      LFO_Chain.Add_Processor (new Transposer'(others => <>));
      return LFO_Chain;
   end LFO;

end Waves;
