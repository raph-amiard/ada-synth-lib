with Ada.Containers.Vectors;

package body Sound_Gen_Interfaces is

   package PA_Vectors
   is new Ada.Containers.Vectors (Natural, Params_Scope);

   Params_Aggregators : PA_Vectors.Vector;

   function Current_FPA return Params_Scope is
     (Params_Aggregators.Last_Element);

   -----------------------------
   -- Register_Note_Generator --
   -----------------------------

   procedure Register_Simulation_Listener
     (N : access I_Simulation_Listener'Class) is
   begin
      Simulation_Listeners (Simulation_Listeners_Nb) := N;
      Simulation_Listeners_Nb := Simulation_Listeners_Nb + 1;
   end Register_Simulation_Listener;

   ---------------
   -- Next_Step --
   ---------------

   procedure Next_Steps is
   begin
      for I in 0 .. Simulation_Listeners_Nb - 1 loop
         Simulation_Listeners (I).Next_Step;
      end loop;
   end Next_Steps;

   ----------------
   -- Base_Reset --
   ----------------

   procedure Base_Reset (Self : in out Generator) is
   begin
      null;
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
      Self.Parameters := new Params_Scope_Type;
      Enter (Self.Parameters);
      Internal (Self);
      Leave (Self.Parameters);
   end Compute_Params;

   -----------
   -- Enter --
   -----------

   procedure Enter (F : Params_Scope) is
   begin
      Params_Aggregators.Append (F);
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave (F : Params_Scope) is
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
        (if G.Parameters /= null
         then Generator_Arrays.To_Array (G.Parameters.Generators)
         else Generator_Arrays.Empty_Array) with Inline_Always;

      function Cat_Arrays
      is new Generator_Arrays.Id_Flat_Map_Gen (Internal);

   begin
      return Internal (Self'Unrestricted_Access)
        & Cat_Arrays (Self.All_Children);
   end Get_Params;

   ----------------------
   -- Set_Scaled_Value --
   ----------------------

   procedure Set_Scaled_Value
     (Self : in out Generator'Class; I : Natural; Val : Scaled_Value_T)
   is
      V : Float :=
        (if Self.Get_Scale (I) = Exp
         then Exp8_Transfer (Float (Val)) else Float (Val));
      Max : constant Float := Self.Get_Max_Value (I);
      Min : constant Float := Self.Get_Min_Value (I);
   begin
      V := V * (Max - Min) + Min;
      Self.Set_Value (I, V);
   end Set_Scaled_Value;

end Sound_Gen_Interfaces;
