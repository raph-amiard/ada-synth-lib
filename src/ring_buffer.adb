package body Ring_Buffer is

   ------------
   -- Create --
   ------------

   function Create (S : Natural) return Ring_Buffer
   is
   begin
      return new Ring_Buffer_Type'(Size => S, others => <>);
   end Create;

   -----------
   -- Write --
   -----------

   procedure Write (RB : in out Ring_Buffer; T : Element_Type)
   is
   begin
      if Checks then
         if RB.Maxed_Writes then
            raise Constraint_Error with "Invalid write in buffer";
         end if;
      end if;

      RB.Maxed_Writes :=
        (RB.Write_Index = (RB.Read_Index - 1) mod RB.Size);

      RB.Ring_Buffer (RB.Write_Index) := T;
      RB.Write_Index := (RB.Write_Index + 1) mod RB.Size;
   end Write;

   ----------
   -- Read --
   ----------

   function Read (RB : in out Ring_Buffer) return Element_Type is
   begin
      if Checks then
         if not RB.Maxed_Writes and then RB.Read_Index = RB.Write_Index then
            raise Constraint_Error with "Invalid read in ring buffer";
         end if;
      end if;

      RB.Maxed_Writes := False;

      return E : Element_Type do
         E := RB.Ring_Buffer (RB.Read_Index);
         RB.Read_Index := (RB.Read_Index + 1) mod RB.Size;
      end return;
   end Read;

   ---------------------------
   -- Available_Read_Frames --
   ---------------------------

   function Available_Read_Frames (RB : Ring_Buffer) return Natural
   is
      WI : constant Natural := (if RB.Write_Index < RB.Read_Index
                                then RB.Write_Index + RB.Size
                                else RB.Write_Index);
   begin

      if RB.Maxed_Writes then
         return RB.Size;
      else
         return WI - RB.Read_Index;
      end if;
   end Available_Read_Frames;

   ----------------------------
   -- Available_Write_Frames --
   ----------------------------

   function Available_Write_Frames (RB : Ring_Buffer) return Natural
   is
      RI : constant Natural := (if RB.Read_Index <= RB.Write_Index
                                then RB.Read_Index + RB.Size
                                else RB.Read_Index);
   begin
      if RB.Maxed_Writes then
         return 0;
      else
         return RI - RB.Write_Index;
      end if;
   end Available_Write_Frames;

end Ring_Buffer;
