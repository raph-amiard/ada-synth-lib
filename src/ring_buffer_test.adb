with GNAT.Random_Numbers; use GNAT.Random_Numbers;
with Ring_Buffer;
with Ada.Text_IO; use Ada.Text_IO;

procedure Ring_Buffer_Test is
   package FRB is new Ring_Buffer
     (Checks => True, Element_Type => Float, Default_Value => 0.0);

   RB : FRB.Ring_Buffer := FRB.Create (16);

   function Random_Len
   is new Random_Discrete (Natural);

   type Float_Array is array (Natural range <>) of Float;

   Input_Array : Float_Array (0 .. 100) := (others => 0.0);
   Output_Array : Float_Array (0 .. 100) := (others => 0.0);
   Input_I, Output_I : Natural := 0;

   G : Generator;
   Tmp : Natural;

   Nb_Runs : constant Natural := 100;
begin

   Reset (G);

   for I in 1 .. Nb_Runs loop
      for El of Input_Array loop
         El := Random (G);
      end loop;

      while Input_I < Input_Array'Length
        and then Output_I < Output_Array'Length
      loop
         Tmp := Random_Len
           (G, Min => 0, Max => FRB.Available_Write_Frames (RB));

         Put_Line ("Available write frames : " & Tmp'Img);

         for Dummy in 1 .. Tmp loop
            exit when Input_I >= Input_Array'Length;
            FRB.Write (RB, Input_Array (Input_I));
            Input_I := Input_I + 1;
         end loop;

         Tmp := Random_Len
           (G, Min => 0, Max => FRB.Available_Read_Frames (RB));

         Put_Line ("Available read frames : " & Tmp'Img);

         for Dummy in 1 .. Tmp loop
            exit when Output_I >= Output_Array'Length;
            Output_Array (Output_I) := FRB.Read (RB);
            Output_I := Output_I + 1;
         end loop;
      end loop;

      for I in Input_Array'Range loop
         pragma Assert (Input_Array (I) = Output_Array (I));
      end loop;

      Put_Line ("Run" & I'Img & " all good ! ");
   end loop;

end Ring_Buffer_Test;
