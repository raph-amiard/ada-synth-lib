with Utils; use Utils;
with Write_To_Stdout_Once;

procedure Write_To_Stdout (G : access Generator'Class) is
begin

   loop
      Write_To_Stdout_Once (G);
      exit when Sample_Nb > 10_000_000;
      Sample_Nb := Sample_Nb + Generator_Buffer_Length;
   end loop;

end Write_To_Stdout;
