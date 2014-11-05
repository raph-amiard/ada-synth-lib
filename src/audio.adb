with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Utils; use Utils;
with Waves; use Waves;
with Effects; use Effects;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

procedure Audio is
   Int_Smp : Short_Integer := 0;
   function Sample_To_Uint16 is new Sample_To_Int (Short_Integer);

   Ignore : Integer;

   Sq     : aliased Square_Generator := Create (Fixed (Note_To_Freq ((A, 5))));
   Sine   : aliased Sine_Generator := Create (Fixed (Note_To_Freq ((A, 3))));
   Saw    : aliased Saw_Generator := Create (Fixed (Note_To_Freq ((G, 4))));
   Sine_3 : aliased Sine_Generator := Create (Fixed (Note_To_Freq ((A, 2))));
   Sine_2 : aliased Sine_Generator :=
     Create (Fixed (Note_To_Freq ((C_Sh, 5)), Proc => LFO (10.0, 100.0)));

   C : aliased Chain := Create (Sine_2'Access);
   C2 : aliased Chain := Create (Sine'Access);
   Main_Mixer : Mixer;

begin
--     Add_Generator (Main_Mixer, Saw'Access, 0.03);
--     Add_Generator (Main_Mixer, Sq'Access, 0.03);

   C.Add_Processor (new Dyn_Attenuator'(Level_Provider => LFO (1.0, 0.5)));
   C2.Add_Processor (new Dyn_Attenuator'(Level_Provider => LFO (3.0, 0.5)));
   Add_Generator (Main_Mixer, C'Access, 0.1);
   Add_Generator (Main_Mixer, Sine_3'Access, 0.1);
   Add_Generator (Main_Mixer, C2'Access, 0.3);
   Put_Line (Standard_Error, Note_To_Freq ((A, 5))'Img);

   loop
      Int_Smp := Sample_To_Uint16 (Next_Sample (Main_Mixer));
      Ignore := GNAT.OS_Lib.Write
        (GNAT.OS_Lib.Standout, Int_Smp'Address, Int_Smp'Size / 8);
   end loop;

end Audio;
