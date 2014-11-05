with Ada.Text_IO; use Ada.Text_IO;

package body Effects is

   function Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float) return Natural
   is
      MG : Mixer_Generator := Mixer_Generator'(Gen   => G,
                                               Level => Level);
   begin
      Self.Generators (Self.Length) := MG;
      Self.Length := Self.Length + 1;
      return Self.Length - 1;
   end Add_Generator;

   procedure Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float)
   is
      Discard : Natural := Add_Generator (Self, G, Level);
   begin
      null;
   end Add_Generator;

   function Next_Sample (Self : in out Mixer) return Sample
   is
      Ret, Tmp : Sample := 0.0;

   begin
      for I in 0 .. Self.Length - 1 loop
         Tmp := Self.Generators (I).Gen.Next_Sample;
         Tmp := Tmp * Sample (Self.Generators (I).Level);
         Ret := Ret + Tmp;
      end loop;
      return Saturate (Ret);
   end Next_Sample;

end Effects;
