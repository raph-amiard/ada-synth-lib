with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;
with Waves; use Waves;

package BLIT is
   Ring_Buf_HB : constant := 1024;
   type Ring_Buffer_T is array (Natural range 0 .. Ring_Buf_HB - 1) of Sample;
   type BLIT_State is (Up, Down);

   type BLIT_Generator is abstract new Wave_Generator with record
      Current_Sample     : Natural;
      Ring_Buffer        : Ring_Buffer_T;
      Next_Impulse_Time  : Natural := 0;
      Next_Impulse_Phase : Float := 0.0;
      Last_Sum           : Sample := 0.0;
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
     (Self : in out BLIT_Square);

   overriding procedure Next_Samples
     (Self : in out BLIT_Saw);

end BLIT;
