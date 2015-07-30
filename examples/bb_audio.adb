with Utils; use Utils;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
pragma Warnings (Off);
with BLIT; use BLIT;
with Interfaces.C; use Interfaces.C;
with Simple_Demo;

procedure Audio is

   Int_Smp : Short_Integer := 0;
   function Sample_To_Uint16 is new Sample_To_Int (Short_Integer);
   Ignore : Integer;
   pragma Unreferenced (Ignore);

   BPM : constant := 120;
begin

   loop
      Next_Steps;
      Simple_Demo.Main_Mixer.Next_Samples;
      for I in B_Range_T'Range loop
         Int_Smp := Sample_To_Uint16 (Simple_Demo.Main_Mixer.Buffer (I));
--           Ignore := GNAT.OS_Lib.Write
--             (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
      end loop;
      exit when Sample_Nb > 10_000_000;
      Sample_Nb := Sample_Nb + Generator_Buffer_Length;
   end loop;

end Audio;
