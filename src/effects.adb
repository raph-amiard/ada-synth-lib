with Config; use Config;

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

         if Self.Saturate then
            Self.Buffer (I) := Saturate (Ret);
         else
            Self.Buffer (I) := Ret;
         end if;

         <<Continue_Label>>
      end loop;
   end Next_Samples;

   ------------------
   -- Create_Mixer --
   ------------------

   function Create_Mixer
     (Sources : Generators_Arg_Array;
      Env : access ADSR := null;
      Saturate : Boolean := True) return access Mixer
   is
      Ret : constant access Mixer := new Mixer;
      Discard : Natural;
   begin

      for Source of Sources loop
         Discard := Add_Generator (Ret.all, Source);
      end loop;

      Ret.Env := Env;
      Ret.Saturate := Saturate;
      return Ret;
   end Create_Mixer;

   ------------
   -- Create --
   ------------

   function Create_LP (Source : access Generator'Class;
                       Cut_Freq : access Generator'Class;
                       Q : Float) return access Low_Pass_Filter
   is
   begin
      return LPF : access Low_Pass_Filter do
         LPF := new Low_Pass_Filter'(Source => Generator_Access (Source),
                                     Cut_Freq_Provider =>
                                       Generator_Access (Cut_Freq),
                                     Res               => Q,
                                     others            => <>);
      end return;
   end Create_LP;

   -----------------
   -- Filter_Init --
   -----------------

   procedure Filter_Init (Self : in out Low_Pass_Filter)
   is
      R, K, K2, BH : Float;
   begin
      Self.Cut_Freq := (if Self.Cut_Freq > 10_000.0 then 10_000.0
                        elsif Self.Cut_Freq < 10.0 then 10.0
                        else Self.Cut_Freq);

      R := 2.0 * (1.0 - Self.Res);
      if R = 0.0 then
         R := 0.001;
      end if;

      K := Filter_Tan (Natural (Self.Cut_Freq));

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
     (Source : access Generator'Class;
      Clip_Level : Float; Coeff : Float := 10.0) return access Disto
   is
   begin
      return new Disto'(Clip_Level => Sample (Clip_Level),
                        Coeff      => Sample (Coeff),
                        Source     => Generator_Access (Source),
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

   -----------------------
   -- Create_Delay_Line --
   -----------------------

   function Create_Delay_Line (Source : access Generator'Class;
                               Dlay : Millisecond;
                               Decay : Sample) return access Delay_Line
   is
   begin
      return new Delay_Line'
        (Source           => Source,
         Delay_In_Samples =>
           B_Range_T (Float (Dlay) * SAMPLE_RATE / 1000.0),
         Decay            => Decay,
         others           => <>);
   end Create_Delay_Line;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples (Self : in out Delay_Line) is
   begin
      Self.Source.Next_Samples;

      Self.Buffer := Self.Source.Buffer;

      for I in 0 .. Self.Delay_In_Samples loop
         Self.Buffer (I) :=
           Self.Buffer (I) +
           (Self.Last_Buffer
              (B_Range_T'Last - Self.Delay_In_Samples + I) * Self.Decay);
      end loop;

      for I in
        B_Range_T'First .. B_Range_T'Last - Self.Delay_In_Samples
      loop
         Self.Buffer (I + Self.Delay_In_Samples) :=
           Self.Buffer (I + Self.Delay_In_Samples) +
             (Self.Buffer (I) * Self.Decay);
      end loop;
      Self.Last_Buffer := Self.Buffer;
   end Next_Samples;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Delay_Line)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Source);
      Self.Last_Buffer := (others => 0.0);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Attenuator)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Source);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Dyn_Attenuator)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Level_Provider);
      Reset_Not_Null (Self.Source);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Transposer)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Source);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Disto)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Source);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Low_Pass_Filter)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Source);
      Reset_Not_Null (Self.Cut_Freq_Provider);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Mixer)
   is
   begin
      Base_Reset (Self);
      Reset_Not_Null (Self.Env);
      for IG in 0 .. Self.Length loop
         Reset_Not_Null (Self.Generators (IG).Gen);
      end loop;
   end Reset;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self : in out Mixer) return Generator_Array is
   begin
      return A : Generator_Array (0 .. Self.Length) do
         for J in 0 .. Self.Length - 1 loop
            A (J) := Self.Generators (J).Gen;
         end loop;
         A (Self.Length) := Self.Env;
      end return;
   end Children;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Self : in out Low_Pass_Filter; I : Natural; Val : Float)
   is
      pragma Unreferenced (I);
   begin
      Self.Res := Val;
   end Set_Value;

end Effects;
