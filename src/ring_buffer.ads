generic
   type Element_Type is private;
   Checks : Boolean := False;
   Default_Value : Element_Type;
package Ring_Buffer is

   type Ring_Buffer is private;

   function Create (S : Natural) return Ring_Buffer;

   procedure Write (RB : in out Ring_Buffer; T : Element_Type)
     with Inline_Always;

   function Read (RB : in out Ring_Buffer) return Element_Type
     with Inline_Always;

   function Available_Read_Frames (RB : Ring_Buffer) return Natural
     with Inline_Always;

   function Available_Write_Frames (RB : Ring_Buffer) return Natural
     with Inline_Always;
private
   type Ring_Buffer_Array is array (Natural range <>) of Element_Type;

   type Ring_Buffer_Type (Size : Natural) is record
      Write_Index, Read_Index : Natural := 0;
      Ring_Buffer             : Ring_Buffer_Array (0 .. Size)
        := (others => Default_Value);
      Maxed_Writes            : Boolean := False;
   end record;

   type Ring_Buffer is access all Ring_Buffer_Type;

end Ring_Buffer;
