with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;
with Waves; use Waves;

package BLIT is
   Ring_Buffer_Size : constant := 16;

   type Ring_Buffer_T is array (0 .. Ring_Buffer_Size - 1) of Sample;
   type BLIT_State is (Up, Down);

   type BLIT_Generator is abstract new Wave_Generator with record
      Current_Sample         : Natural;
      Ring_Buffer            : Ring_Buffer_T;
      Next_Impulse_Time      : Natural := 0;
      Next_Impulse_Phase     : Float := 0.0;
      Remaining_BLIT_Samples : Natural := 0;
      Last_Sum               : Sample := 0.0;
   end record;

   overriding function Children
     (Self : in out BLIT_Generator) return Generator_Array
   is (Empty_Generator_Array);

   type BLIT_Square is new BLIT_Generator with record
      State : BLIT_State := Down;
   end record;

   type BLIT_Saw is new BLIT_Generator with null record;

   overriding procedure Reset (Self : in out BLIT_Square);
   overriding procedure Reset (Self : in out BLIT_Saw);

   function Create_Square
     (Freq_Provider : access Generator'Class) return access BLIT_Square;

   function Create_Saw
     (Freq_Provider : access Generator'Class) return access BLIT_Saw;

   overriding procedure Next_Samples
     (Self : in out BLIT_Square; Buffer : in out Generator_Buffer);

   overriding procedure Next_Samples
     (Self : in out BLIT_Saw; Buffer : in out Generator_Buffer);

end BLIT;
