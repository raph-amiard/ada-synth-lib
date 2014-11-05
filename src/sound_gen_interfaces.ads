with Utils; use Utils;

package Sound_Gen_Interfaces is

   ---------------
   -- Generator --
   ---------------

   type Generator is interface;
   function Next_Sample (Self : in out Generator) return Sample is abstract;
   type Generator_Access is access all Generator'Class;

   ----------------------
   -- Signal_Processor --
   ----------------------

   type Signal_Processor is interface;
   function Process
     (Self : in out Signal_Processor; S : Sample) return Sample is abstract;
   type Signal_Processor_Access is access all Signal_Processor'Class;

end Sound_Gen_Interfaces;
