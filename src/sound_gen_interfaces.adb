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

   procedure Next_Step is
   begin
      for I in 0 .. Note_Generators_Nb - 1 loop
         Note_Generators (I).Next_Message;
      end loop;
   end Next_Step;

end Sound_Gen_Interfaces;
