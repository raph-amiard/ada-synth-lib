pragma Warnings (Off);

with Config; use Config;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

pragma Warnings (On);

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

   overriding procedure Next_Samples (Self : in out Mixer)
   is
      Ret, Tmp : Sample := 0.0;
      Env_Level : Sample;
   begin
      if Self.Env /= null then
         Self.Env.Next_Samples;
      end if;

      for I in 0 .. Self.Length - 1 loop
         Self.Generators (I).Gen.Next_Samples;
      end loop;

      for I in B_Range_T'Range loop
         Ret := 0.0;
         if Self.Env /= null then
            Env_Level := Self.Env.Buffer (I);
            if Env_Level = 0.0 then
               Self.Buffer (I) := 0.0;
               goto Continue_Label;
            end if;
         end if;

         for J in 0 .. Self.Length - 1 loop
            Tmp := Self.Generators (J).Gen.Buffer (I);
            Tmp := Tmp * Sample (Self.Generators (J).Level);
            Ret := Ret + Tmp;
         end loop;

         if Self.Env /= null then
            Ret := Ret * Env_Level;
         end if;

         Self.Buffer (I) := Saturate (Ret);

         <<Continue_Label>>
      end loop;
   end Next_Samples;

   ------------------
   -- Create_Mixer --
   ------------------

   function Create_Mixer
     (Sources : Generators_Arg_Array;
      Env : access ADSR := null) return access Mixer
   is
      Ret : constant access Mixer := new Mixer;
      Discard : Natural;
      pragma Unreferenced (Discard);
   begin
      for Source of Sources loop
         Discard := Add_Generator (Ret.all, Source);
      end loop;
      Ret.Env := Env;
      return Ret;
   end Create_Mixer;

   ------------
   -- Create --
   ------------

   function Create_LP (Source : Generator_Access;
                       Cut_Freq_Provider : Generator_Access;
                       Res : Float) return access Low_Pass_Filter
   is
   begin
      return new Low_Pass_Filter'(Source => Source,
                                  Cut_Freq_Provider => Cut_Freq_Provider,
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

      K := Utils.Tan (W * Pi);

      Put (Standard_Error, "INPUT OF TAN : ");
      Put (Standard_Error, W * Pi, 2, 5, 0);
      New_Line (Standard_Error);

      Put (Standard_Error, "ERROR OF TAN : ");
      Put (Standard_Error, (K - Utils.Tan (W * Pi)), 2, 5, 0);
      New_Line (Standard_Error);

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

   overriding procedure Next_Samples
     (Self : in out Low_Pass_Filter)
   is
      X, Y : Float;
      Cut_Freq : Float;
   begin
      Self.Cut_Freq_Provider.Next_Samples;
      Self.Source.Next_Samples;

      for I in B_Range_T'Range loop
         Cut_Freq := Float (Self.Cut_Freq_Provider.Buffer (I));
         if Cut_Freq /= Self.Cut_Freq then
            Self.Cut_Freq := Cut_Freq;
            Filter_Init (Self);
         end if;

         X := 0.7 * Float (Self.Source.Buffer (I));
         Y := Self.A0 * X + Self.D1;
         Self.D1 := Self.D2 + Self.A1 * X + Self.B1 * Y;
         Self.D2 := Self.A2 * X + Self.B2 * Y;
         X := Y;
         --  and the second
         Y := Self.A0 * X + Self.D3;
         Self.D3 := Self.D4 + Self.A1 * X + Self.B1 * Y;
         Self.D4 := Self.A2 * X + Self.B2 * Y;

         Self.Buffer (I) := Sample (Y);
      end loop;
   end Next_Samples;

   ----------------------
   -- Create_Digi_Dist --
   ----------------------

   function Create_Digi_Dist
     (Clip_Level : Float) return access Digital_Disto is
   begin
      return new Digital_Disto'(Clip_Level => Sample (Clip_Level));
   end Create_Digi_Dist;

   -------------
   -- Process --
   -------------

   overriding function Process
     (Self : in out Digital_Disto; S : Sample) return Sample is
   begin
      return (if S > Self.Clip_Level then Self.Clip_Level
              elsif S < -Self.Clip_Level then -Self.Clip_Level
              else S);
   end Process;

   -----------------
   -- Create_Dist --
   -----------------

   function Create_Dist
     (Source : Generator_Access;
      Clip_Level : Float; Coeff : Float := 10.0) return access Disto
   is
   begin
      return new Disto'(Clip_Level => Sample (Clip_Level),
                        Coeff      => Sample (Coeff),
                        Source     => Source,
                        others => <>);
   end Create_Dist;

   -------------
   -- Process --
   -------------

   overriding procedure Next_Samples
     (Self : in out Disto)
   is
      S : Sample;
      Invert : Boolean;
   begin
      Self.Source.Next_Samples;

      for I in B_Range_T'Range loop
         Invert := False;
         S := Self.Source.Buffer (I) * 0.5;
         if S < 0.0 then
            S := -S;
            Invert := True;
         end if;

         if S > 1.0 then
            S := (Self.Clip_Level + 1.0) / 2.0;
         elsif S > Self.Clip_Level then
            S := S * Self.Coeff;
            S :=
              Self.Clip_Level +
                ((S - Self.Clip_Level) /
                 (1.0 + ((S - Self.Clip_Level) /
                    (1.0 - Self.Clip_Level)) ** 2));
         end if;

         if Invert then
            S := -S;
         end if;
         Self.Buffer (I) := S * 2.0;
--             (if S > Self.Clip_Level
--              then Self.Clip_Level + ((S - Self.Clip_Level) / Self.Coeff)
--              elsif S < -Self.Clip_Level
--              then -Self.Clip_Level - ((S + Self.Clip_Level) / Self.Coeff)
--              else S);
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Attenuator)
   is
   begin
      Self.Source.Next_Samples;
      for I in B_Range_T'Range loop
         Self.Buffer (I) := (Self.Source.Buffer (I) * Sample (Self.Level));
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Dyn_Attenuator)
   is
   begin
      Self.Source.Next_Samples;
      Self.Level_Provider.Next_Samples;
      for I in B_Range_T'Range loop
         Self.Buffer (I) :=
           Self.Source.Buffer (I) * Self.Level_Provider.Buffer (I);
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Transposer)
   is
   begin
      Self.Source.Next_Samples;
      for I in B_Range_T'Range loop
         Self.Buffer (I) := (Self.Source.Buffer (I) + 1.0) / 2.0;
      end loop;
   end Next_Samples;

end Effects;
