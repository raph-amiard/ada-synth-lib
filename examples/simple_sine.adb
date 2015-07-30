with Waves; use Waves;
with Write_To_Stdout;

procedure Simple_Sine is
   Sine_Gen : constant access Sine_Generator :=
      Create_Sine (Fixed (440.0));
begin
   Write_To_Stdout (Sine_Gen);
end Simple_Sine;
