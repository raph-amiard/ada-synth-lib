with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Waves; use Waves;
with Write_To_Stdout;

procedure Simple_Sine_2 is
   Sine_Gen : constant Generator_Access :=
     Create_Sine (Fixed (1000.0, LFO (6.0, 200.0)));
begin
   Write_To_Stdout (Sine_Gen);
end Simple_Sine_2;
