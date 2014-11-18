package body Sound_Gen_Interfaces is

   -----------------------------
   -- Register_Note_Generator --
   -----------------------------

   procedure Register_Note_Generator (N : Note_Generator_Access) is
   begin
      Note_Generators (Note_Generators_Nb) := N;
      Note_Generators_Nb := Note_Generators_Nb + 1;
   end Register_Note_Generator;

   ---------------
   -- Next_Step --
   ---------------

   procedure Next_Steps is
   begin
      for I in 0 .. Note_Generators_Nb - 1 loop
         Note_Generators (I).Next_Messages;
      end loop;
   end Next_Steps;

end Sound_Gen_Interfaces;
