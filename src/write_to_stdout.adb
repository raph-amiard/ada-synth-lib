with Utils; use Utils;
with GNAT.OS_Lib;
with Interfaces; use Interfaces;

procedure Write_To_Stdout (G : access Generator'Class)
is
   function Sample_To_Int16 is new Sample_To_Int (Short_Integer);
   --  function Sample_To_UInt16 is new Sample_To_UInt (Unsigned_16);
   Int_Smp : Short_Integer := 0;
   Ignore : Integer;
begin

   loop
      Next_Steps;
      G.Next_Samples;

      for I in B_Range_T'Range loop
         Int_Smp := Sample_To_Int16 (G.Buffer (I));
         Ignore := GNAT.OS_Lib.Write
           (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
      end loop;

      exit when Sample_Nb > 10_000_000;
      Sample_Nb := Sample_Nb + Generator_Buffer_Length;
   end loop;

end Write_To_Stdout;
