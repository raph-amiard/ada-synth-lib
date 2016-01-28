with Ada.Containers.Vectors;

package body Sound_Gen_Interfaces is

   package PA_Vectors
   is new Ada.Containers.Vectors (Natural, Params_Aggregator);

   Params_Aggregators : PA_Vectors.Vector;

   function Current_FPA return Params_Aggregator is
     (Params_Aggregators.Last_Element);

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

   ----------------
   -- Base_Reset --
   ----------------

   procedure Base_Reset (Self : in out Generator) is
   begin
      Self.Buffer := (others => 0.0);
   end Base_Reset;

   --------------------
   -- Reset_Not_Null --
   --------------------

   procedure Reset_Not_Null (Self : Generator_Access) is
   begin
      if Self /= null then
         Self.Reset;
      end if;
   end Reset_Not_Null;

   --------------------
   -- Reset_Not_Null --
   --------------------

   procedure Reset_Not_Null (Self : Note_Generator_Access) is
   begin
      if Self /= null then
         Self.Reset;
      end if;
   end Reset_Not_Null;

   --------------------------
   -- Compute_Fixed_Params --
   --------------------------

   procedure Compute_Params (Self : in out Generator) is

      procedure Internal (Self : in out Generator'Class);
      procedure Internal (Self : in out Generator'Class) is
      begin
         for C of Self.Children loop
            if C /= null then
               if C.Is_Param then
                  Add_To_Current (C);
               end if;
               Internal (C.all);
            end if;
         end loop;
      end Internal;

   begin
      Self.Params_Scope := new Params_Aggregator_Type;
      Enter (Self.Params_Scope);
      Internal (Self);
      Leave (Self.Params_Scope);
   end Compute_Params;

   -----------
   -- Enter --
   -----------

   procedure Enter (F : Params_Aggregator) is
   begin
      Params_Aggregators.Append (F);
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave (F : Params_Aggregator) is
   begin
      pragma Assert (F = Current_FPA);
      Params_Aggregators.Delete_Last;
   end Leave;

   --------------------
   -- Add_To_Current --
   --------------------

   procedure Add_To_Current (G : Generator_Access) is
      use Ada.Containers;
   begin
      if Params_Aggregators.Length > 0 then
         Current_FPA.Generators.Append (G);
      end if;
   end Add_To_Current;

   ------------------
   -- All_Children --
   ------------------

   function All_Children
     (Self : in out Generator) return Generator_Array
   is
      function All_Children_Internal
        (G : Generator_Access) return Generator_Array
      is
        (G.All_Children) with Inline_Always;

      function Is_Null (G : Generator_Access) return Boolean
      is (G /= null) with Inline_Always;

      function Cat_Arrays
      is new Generator_Arrays.Id_Flat_Map_Gen (All_Children_Internal);

      function Filter_Null is new Generator_Arrays.Filter_Gen (Is_Null);

      S : Generator'Class := Self;
      use Generator_Arrays;
   begin
      return Filter_Null (S.Children & Cat_Arrays (Filter_Null (S.Children)));
   end All_Children;

   ----------------
   -- Get_Params --
   ----------------

   function Get_Params
     (Self : in out Generator) return Generator_Arrays.Array_Type
   is
      use Generator_Arrays;

      function Internal
        (G : Generator_Access) return Generator_Arrays.Array_Type
      is
        (if G.Params_Scope /= null
         then Generator_Arrays.To_Array (G.Params_Scope.Generators)
         else Generator_Arrays.Empty_Array) with Inline_Always;

      function Cat_Arrays
      is new Generator_Arrays.Id_Flat_Map_Gen (Internal);

   begin
      return Internal (Self'Unrestricted_Access)
        & Cat_Arrays (Self.All_Children);
   end Get_Params;

end Sound_Gen_Interfaces;
