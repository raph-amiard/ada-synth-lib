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
      pragma Suppress (Accessibility_Check);
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

   overriding procedure Next_Samples
     (Self : in out Mixer; Buffer : in out Generator_Buffer)
   is
      Work_Buffer : Generator_Buffer renames Self.Work_Buffer;
   begin
      --  Set every element of the buffer to 0.0
      Buffer := (others => 0.0);

      --  For every channel
      for I in 0 .. Self.Length - 1 loop

         --  Compute the samples of the channel's generator
         Self.Generators (I).Gen.Next_Samples (Work_Buffer);

         --  And add the samples to the buffer
         for J in Buffer'Range loop
            Buffer (J) :=
              Buffer (J)
              + (Work_Buffer (J) * Sample (Self.Generators (I).Level));
         end loop;
      end loop;

      if Self.Env /= null then
         Self.Env.Next_Samples (Work_Buffer);
         for J in Buffer'Range loop
            Buffer (J) := Buffer (J) * Work_Buffer (J);
         end loop;
      elsif Self.Env /= null and then Self.Saturate then
         Self.Env.Next_Samples (Work_Buffer);
         for J in Buffer'Range loop
            Buffer (J) := Saturate (Buffer (J) * Work_Buffer (J));
         end loop;
      elsif Self.Saturate then
         for J in Buffer'Range loop
            Buffer (J) := Saturate (Buffer (J));
         end loop;
      end if;

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
     (Self : in out Low_Pass_Filter; Buffer : in out Generator_Buffer)
   is
      X, Y : Float;
      Cut_Freq : Float;
      Tmp_Buffer : Generator_Buffer;
   begin
      Self.Cut_Freq_Provider.Next_Samples (Tmp_Buffer);

      Self.Source.Next_Samples (Buffer);

      for I in Buffer'Range loop
         Cut_Freq := Float (Tmp_Buffer (I));
         if Cut_Freq /= Self.Cut_Freq then
            Self.Cut_Freq := Cut_Freq;
            Filter_Init (Self);
         end if;

         X := 0.7 * Float (Buffer (I));
         Y := Self.A0 * X + Self.D1;
         Self.D1 := Self.D2 + Self.A1 * X + Self.B1 * Y;
         Self.D2 := Self.A2 * X + Self.B2 * Y;
         X := Y;
         --  and the second
         Y := Self.A0 * X + Self.D3;
         Self.D3 := Self.D4 + Self.A1 * X + Self.B1 * Y;
         Self.D4 := Self.A2 * X + Self.B2 * Y;

         Buffer (I) := Sample (Y);
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
     (Self : in out Disto; Buffer : in out Generator_Buffer)
   is
      S : Sample;
      Invert : Boolean;
   begin
      Self.Source.Next_Samples (Buffer);

      for I in Buffer'Range loop
         Invert := False;
         S := Buffer (I) * 0.5;
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
         Buffer (I) := S * 2.0;
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
     (Self : in out Attenuator; Buffer : in out Generator_Buffer)
   is
   begin
      Self.Source.Next_Samples (Buffer);
      for I in Buffer'Range loop
         Buffer (I) := (Buffer (I) * Sample (Self.Level));
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Dyn_Attenuator; Buffer : in out Generator_Buffer)
   is
      Tmp_Buffer : Generator_Buffer;
   begin
      Self.Source.Next_Samples (Buffer);

      Self.Level_Provider.Next_Samples (Tmp_Buffer);

      for I in Buffer'Range loop
         Buffer (I) := Buffer (I) * Tmp_Buffer (I);
      end loop;
   end Next_Samples;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Transposer; Buffer : in out Generator_Buffer)
   is
   begin
      Self.Source.Next_Samples (Buffer);
      for I in Buffer'Range loop
         Buffer (I) := (Buffer (I) + 1.0) / 2.0;
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
           Buffer_Range_Type (Float (Dlay) * SAMPLE_RATE / 1000.0),
         Decay            => Decay,
         others           => <>);
   end Create_Delay_Line;

   ------------------
   -- Next_Samples --
   ------------------

   overriding procedure Next_Samples
     (Self : in out Delay_Line; Buffer : in out Generator_Buffer)
   is
   begin
      Self.Source.Next_Samples (Buffer);

      for I in 0 .. Self.Delay_In_Samples loop
         Buffer (I) :=
           Buffer (I) +
           (Self.Last_Buffer
              (Buffer_Range_Type'Last - Self.Delay_In_Samples + I)
            * Self.Decay);
      end loop;

      for I in
        Buffer_Range_Type'First ..
          Buffer_Range_Type'Last - Self.Delay_In_Samples
      loop
         Buffer (I + Self.Delay_In_Samples) :=
           Buffer (I + Self.Delay_In_Samples) +
             (Buffer (I) * Self.Decay);
      end loop;
      Self.Last_Buffer := Buffer;
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

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Self : in out Attenuator; I : Natural; Val : Float) is
      pragma Unreferenced (I);
   begin
      Self.Level := Val;
   end Set_Value;

end Effects;
