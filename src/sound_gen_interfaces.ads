with Utils; use Utils;

package Sound_Gen_Interfaces is

   Sample_Nb : Sample_Period := 1;

   ---------------
   -- Generator --
   ---------------

   Generator_Buffer_Length : constant := 1024;
   type B_Range_T is range 0 .. Generator_Buffer_Length - 1;

   type Generator_Buffer is
     array (B_Range_T) of Sample;

   type Generator is abstract tagged record
      Buffer : Generator_Buffer;
   end record;
   type Generator_Access is access all Generator'Class;

   procedure Next_Samples (Self : in out Generator) is abstract;
   pragma Inline (Next_Samples);

   procedure Base_Reset (Self : in out Generator);
   procedure Reset_Not_Null (Self : Generator_Access);

   procedure Reset (Self : in out Generator) is abstract;

   ----------------------
   -- Signal_Processor --
   ----------------------

   type Signal_Processor is interface;
   function Process
     (Self : in out Signal_Processor; S : Sample) return Sample is abstract;
   type Signal_Processor_Access is access all Signal_Processor'Class;

   -------------
   -- Trigger --
   -------------

   type Note_Signal_T is (On, Off, No_Signal);

   type Note_Signal is record
      Note : Note_T;
      Kind : Note_Signal_T := No_Signal;
   end record;

   type Note_Signal_Buffer is array (B_Range_T) of Note_Signal;
   type Note_Generator is abstract tagged record
      Current_Sample_Nb : Sample_Period := 0;
      Buffer            : Note_Signal_Buffer;
   end record;
   type Note_Generator_Access is access all Note_Generator'Class;

   procedure Next_Messages (Self : in out Note_Generator) is abstract;
   procedure Reset (Self : in out Note_Generator) is abstract;
   procedure Reset_Not_Null (Self : Note_Generator_Access);

   type Note_Generator_Array is
     array (Natural range <>) of Note_Generator_Access;

   Note_Generators    : Note_Generator_Array (0 .. 64);
   Note_Generators_Nb : Natural := 0;

   procedure Register_Note_Generator (N : Note_Generator_Access);

   procedure Next_Steps;

end Sound_Gen_Interfaces;
