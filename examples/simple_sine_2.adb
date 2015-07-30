with Waves; use Waves;
with Write_To_Stdout;

procedure Simple_Sine_2 is
   Sine_Gen : constant access Sine_Generator :=
     Create_Sine (Fixed (1000.0, LFO (6.0, 200.0)));
begin
   Write_To_Stdout (Sine_Gen);
end Simple_Sine_2;
