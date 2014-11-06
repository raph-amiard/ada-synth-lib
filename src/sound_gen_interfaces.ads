with Utils; use Utils;

package Sound_Gen_Interfaces is

   Sample_Nb : Period := 1;

   ---------------
   -- Generator --
   ---------------

   type Generator is abstract tagged record
      Current_Sample_Nb : Period := 0;
      Memo_Sample : Sample;
   end record;
   type Generator_Access is access all Generator'Class;

   function Next_Sample_Impl (Self : in out Generator) return Sample is abstract;

   function Next_Sample (Self : in out Generator) return Sample;
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

   type Note_Signal (Kind : Note_Signal_T := No_Signal) is record
      case Kind is
         when On =>
            Note : Note_T;
         when others => null;
      end case;
   end record;

   type Note_Generator is abstract tagged record
      Current_Sample_Nb : Period := 0;
      Memo_Signal : Note_Signal;
   end record;
   type Note_Generator_Access is access all Note_Generator'Class;

   function Next_Message_Impl (Self : in out Note_Generator) return Note_Signal is abstract;
   function Next_Message (Self : in out Note_Generator) return Note_Signal;

end Sound_Gen_Interfaces;
