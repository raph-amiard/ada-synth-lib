with Ada.Numerics.Elementary_Functions;
use Ada.Numerics, Ada.Numerics.Elementary_Functions;

package body BLIT is
   Low_Pass : constant := 0.999;
   --  lower values filter more high frequency

   Phase_Count : constant := 32;
   --  number of phase offsets to sample band-limited step at

   Step_Width : constant := 16;
   --  number of samples in each final band-limited step

   Steps : array (Natural range 0 .. Phase_Count - 1,
                  Natural range 0 .. Step_Width - 1) of Sample;
   --  would use short for speed in a real program

   procedure Init_Steps;
   procedure Add_Step
     (Self : in out BLIT_Generator;
      Time : Natural; Phase : Float; Delt : Sample);

   ----------------
   -- Init_Steps --
   ----------------

   procedure Init_Steps
   is
      --  Generate master band-limited step by adding sine components of a
      --  square wave
      Master_Size : constant := Step_Width * Phase_Count;
      Master : array (Natural range 0 .. Master_Size - 1) of Float :=
        (others => 0.5);
      Gain : Float := 0.5 / 0.777;
      --  adjust normal square wave's amplitude of ~0.777 to 0.5

      Sine_Size : constant Integer := 256 * Phase_Count + 2;
      Max_Harmonic : constant Integer := Sine_Size / 2 / Phase_Count;
      H           : Natural := 1;
   begin
      loop
         exit when H > Max_Harmonic;
         declare
            Amplitude : constant Float := Gain / Float (H);
            To_Angle : constant Float :=
              Utils.Pi * 2.0 / Float (Sine_Size) *
              Float (H);
         begin
            for I in 0 .. Master_Size - 1 loop
               Master (I) := Master (I) +
                 Elementary_Functions.Sin
                   (Float (I - Master_Size / 2) * To_Angle) * Amplitude;
            end loop;
            Gain := Gain * Low_Pass;
         end;
         H := H + 2;
      end loop;

      for Phase in 0 .. Phase_Count - 1 loop
         declare
            Error : Long_Float := 1.0;
            Prev : Long_Float := 0.0;
         begin
            for I in 0 .. Step_Width - 1 loop
               declare
                  Cur : constant Long_Float := Long_Float
                    (Master (I * Phase_Count + (Phase_Count - 1 - Phase)));
                  Delt : constant Long_Float := Cur - Prev;
               begin
                  Error := Error - Delt;
                  Prev := Cur;
                  Steps (Phase, I) := Sample (Delt);
               end;
            end loop;
            Steps (Phase, Step_Width / 2) :=
              Steps (Phase, Step_Width / 2) + Sample (Error * 0.5);
            Steps (Phase, Step_Width / 2 + 1) :=
              Steps (Phase, Step_Width / 2 + 1) + Sample (Error * 0.5);
         end;
      end loop;
   end Init_Steps;

   -------------------
   -- Create_Square --
   -------------------

   function Create_Square
     (Freq_Provider : access Generator'Class) return access BLIT_Square is
   begin
      return new BLIT_Square'(Frequency_Provider =>
                                Generator_Access (Freq_Provider),
                              Current_Sample => 0,
                              others => <>);
   end Create_Square;

   ----------------
   -- Create_Saw --
   ----------------

   function Create_Saw
     (Freq_Provider : access Generator'Class) return access BLIT_Saw is
   begin
      return new BLIT_Saw'(Frequency_Provider =>
                             Generator_Access (Freq_Provider),
                              Current_Sample => 0,
                              others => <>);
   end Create_Saw;

   --------------
   -- Add_Step --
   --------------

   procedure Add_Step (Self : in out BLIT_Generator;
                       Time : Natural; Phase : Float; Delt : Sample)
   is
      P : constant Natural :=
            Natural (Float'Floor (Phase * Float (Phase_Count)));
   begin
      for I in 0 .. Step_Width - 1 loop
         Self.Ring_Buffer ((Time + I) mod Ring_Buf_HB) := Steps (P, I) * Delt;
      end loop;
   end Add_Step;

   -----------------
   -- Next_Sample --
   -----------------

   overriding procedure Next_Samples
     (Self : in out BLIT_Square)
   is
      Impulse_Time  : Natural;
      Impulse_Phase : Float;
      Delta_Time    : Float;
      CSample_Nb    : Natural;

   begin
      Update_Period (Self);

      for I in B_Range_T'Range loop
         CSample_Nb := Natural (Sample_Nb) + Natural (I);
         if Self.Next_Impulse_Time <= CSample_Nb then
            Impulse_Time := Self.Next_Impulse_Time;
            Impulse_Phase := Self.Next_Impulse_Phase;

            Delta_Time := Float (Self.P_Buffer (I)) / 2.0
              + Self.Next_Impulse_Phase;

            Self.Next_Impulse_Time := Self.Next_Impulse_Time +
              Natural (Float'Floor (Delta_Time));
            Self.Next_Impulse_Phase := Delta_Time - Float'Floor (Delta_Time);

            for I in Impulse_Time .. Self.Next_Impulse_Time loop
               Self.Ring_Buffer (I mod Ring_Buf_HB) := 0.0;
            end loop;

            if Self.State = Up then
               Add_Step
                 (BLIT_Generator (Self), Impulse_Time, Impulse_Phase, -1.0);
               Self.State := Down;
            else
               Add_Step
                 (BLIT_Generator (Self), Impulse_Time, Impulse_Phase, 1.0);
               Self.State := Up;
            end if;
         end if;

         Self.Last_Sum :=
           Self.Last_Sum +
             Self.Ring_Buffer (CSample_Nb mod Ring_Buf_HB);

         Self.Buffer (I) :=  Self.Last_Sum - 0.5;
      end loop;
   end Next_Samples;

   -----------------
   -- Next_Sample --
   -----------------

   overriding procedure Next_Samples
     (Self : in out BLIT_Saw)
   is
      Impulse_Time  : Natural;
      Impulse_Phase : Float;
      Delta_Time    : Float;
      CSample_Nb    : Natural;
   begin
      Update_Period (Self);

      for I in B_Range_T'Range loop
         CSample_Nb := Natural (Sample_Nb) + Natural (I);

         if Self.Next_Impulse_Time <= CSample_Nb then
            Impulse_Time := Self.Next_Impulse_Time;
            Impulse_Phase := Self.Next_Impulse_Phase;

            Delta_Time := Float (Self.P_Buffer (I)) + Self.Next_Impulse_Phase;

            Self.Next_Impulse_Time := Self.Next_Impulse_Time +
              Natural (Float'Floor (Delta_Time));
            Self.Next_Impulse_Phase := Delta_Time - Float'Floor (Delta_Time);

            for I in Impulse_Time + Step_Width .. Self.Next_Impulse_Time loop
               Self.Ring_Buffer (I mod Ring_Buf_HB) := 0.0;
            end loop;

            Add_Step (BLIT_Generator (Self), Impulse_Time, Impulse_Phase, 1.0);
         end if;

         Self.Last_Sum :=
           Self.Last_Sum +
             Self.Ring_Buffer (CSample_Nb mod Ring_Buf_HB);

         Self.Last_Sum := Self.Last_Sum
           - (Self.Last_Sum
              / (Sample (Self.Next_Impulse_Time - CSample_Nb)));

         if Self.Last_Sum'Valid then
            Self.Buffer (I) := Self.Last_Sum - 0.5;
         else
            Self.Last_Sum := 0.0;
         end if;
      end loop;
   end Next_Samples;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out BLIT_Square) is
   begin
      Base_Reset (Self);
      Self.Ring_Buffer := (others => 0.0);
      Self.Next_Impulse_Time := 0;
      Self.Next_Impulse_Phase := 0.0;
      Self.Last_Sum := 0.0;
      Self.Current_Sample := 0;
      Self.State := Down;
      Reset_Not_Null (Self.Frequency_Provider);
      Self.P_Buffer := (others => 0.0);
   end Reset;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out BLIT_Saw) is
   begin
      Base_Reset (Self);
      Self.Ring_Buffer := (others => 0.0);
      Self.Next_Impulse_Time := 0;
      Self.Next_Impulse_Phase := 0.0;
      Self.Last_Sum := 0.0;
      Self.Current_Sample := 0;
      Reset_Not_Null (Self.Frequency_Provider);
      Self.P_Buffer := (others => 0.0);
   end Reset;

begin
   Init_Steps;
end BLIT;
