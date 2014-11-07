with Config; use Config;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

package body Effects is

   function Add_Generator
     (Self : in out Mixer; G : Mixer_Generator) return Natural;

   procedure Filter_Init (Self : in out Low_Pass_Filter);

   -------------------
   -- Add_Generator --
   -------------------

   function Add_Generator
     (Self : in out Mixer; G : Mixer_Generator) return Natural
   is
   begin
      Self.Generators (Self.Length) := G;
      Self.Length := Self.Length + 1;
      return Self.Length - 1;
   end Add_Generator;

   -------------------
   -- Add_Generator --
   -------------------

   function Add_Generator
     (Self : in out Mixer; G : access Generator'Class;
      Level : Float) return Natural
   is
      MG : constant Mixer_Generator := Mixer_Generator'(Gen   => G,
                                               Level => Level);
   begin
      return Add_Generator (Self, MG);
   end Add_Generator;

   -------------------
   -- Add_Generator --
   -------------------

   procedure Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float)
   is
      Discard : constant Natural := Add_Generator (Self, G, Level);
      pragma Unreferenced (Discard);
   begin
      null;
   end Add_Generator;

   ----------------------
   -- Next_Sample_Impl --
   ----------------------

   overriding function Next_Sample_Impl (Self : in out Mixer) return Sample
   is
      Ret, Tmp : Sample := 0.0;

   begin
      for I in 0 .. Self.Length - 1 loop
         Tmp := Self.Generators (I).Gen.Next_Sample;
         Tmp := Tmp * Sample (Self.Generators (I).Level);
         Ret := Ret + Tmp;
      end loop;
      return Saturate (Ret);
   end Next_Sample_Impl;

   ------------------
   -- Create_Mixer --
   ------------------

   function Create_Mixer
     (Sources : Generators_Arg_Array) return access Mixer
   is
      Ret : constant access Mixer := new Mixer;
      Discard : Natural;
      pragma Unreferenced (Discard);
   begin
      for Source of Sources loop
         Discard := Add_Generator (Ret.all, Source);
      end loop;
      return Ret;
   end Create_Mixer;

   ------------
   -- Create --
   ------------

   function Create_LP (Cut_Freq_Provider : Generator_Access;
                    Res : Float) return access Low_Pass_Filter
   is
   begin
      return new Low_Pass_Filter'(Cut_Freq_Provider => Cut_Freq_Provider,
                                  Res => Res,
                                  others => <>);
   end Create_LP;

   -----------------
   -- Filter_Init --
   -----------------

   procedure Filter_Init (Self : in out Low_Pass_Filter)
   is
      W, R, K, K2, BH : Float;
   begin
      Self.Cut_Freq := (if Self.Cut_Freq > 10_000.0 then 10_000.0
                        elsif Self.Cut_Freq < 10.0 then 10.0
                        else Self.Cut_Freq);

      W := Self.Cut_Freq / SAMPLE_RATE;
      R := 2.0 * (1.0 - Self.Res);
      if R = 0.0 then
         R := 0.001;
      end if;

      K := Tan (W * Pi);
      K2 := K * K;
      BH := 1.0 + R * K + K2;
      Self.A0 := K2 / BH;
      Self.A2 := K2 / BH;
      Self.A1 := Self.A0 * 2.0;
      Self.B1 := -2.0 * (K2 - 1.0) / BH;
      Self.B2 := -(1.0 - (R * K) + K2) / BH;
   end Filter_Init;

   -------------
   -- Process --
   -------------

   overriding function Process
     (Self : in out Low_Pass_Filter; S : Sample) return Sample
   is
      X, Y : Float;
      Cut_Freq : Float;
   begin
      Cut_Freq := Float (Self.Cut_Freq_Provider.Next_Sample);
      if Cut_Freq /= Self.Cut_Freq then
         Self.Cut_Freq := Cut_Freq;
         Filter_Init (Self);
      end if;

      X := 0.7 * Float (S);
      Y := Self.A0 * X + Self.D1;
      Self.D1 := Self.D2 + Self.A1 * X + Self.B1 * Y;
      Self.D2 := Self.A2 * X + Self.B2 * Y;
      X := Y;
      --  and the second
      Y := Self.A0 * X + Self.D3;
      Self.D3 := Self.D4 + Self.A1 * X + Self.B1 * Y;
      Self.D4 := Self.A2 * X + Self.B2 * Y;

      return Sample (Y);
   end Process;

end Effects;
