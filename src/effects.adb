package body Effects is

   function Add_Generator
     (Self : in out Mixer; G : Mixer_Generator) return Natural;

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

end Effects;
