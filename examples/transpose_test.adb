with Ada.Text_IO; use Ada.Text_IO;

with Utils; use Utils;

procedure Transpose_Test is
   N : Note_T := (A, 4);
begin
   Put_Line (Note_Img (N));

   for I in 1 .. 15 loop
      N := Transpose (N, 1);
      Put_Line (Note_Img (N));
   end loop;

   Put_Line ("-----------------------");

   for I in 1 .. 15 loop
      N := Transpose (N, -1);
      Put_Line (Note_Img (N));
   end loop;

   Put_Line ("-----------------------");

   for I in 1 .. 15 loop
      N := Transpose (N, 3);
      Put_Line (Note_Img (N));
   end loop;

   Put_Line ("-----------------------");

   for I in 1 .. 15 loop
      N := Transpose (N, -3);
      Put_Line (Note_Img (N));
   end loop;

   Put_Line ("-----------------------");

   for I in 1 .. 4 loop
      N := Transpose (N, 12);
      Put_Line (Note_Img (N));
   end loop;

   Put_Line ("-----------------------");

   for I in 1 .. 4 loop
      N := Transpose (N, -12);
      Put_Line (Note_Img (N));
   end loop;
end Transpose_Test;
