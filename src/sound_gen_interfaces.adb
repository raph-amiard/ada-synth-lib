package body Sound_Gen_Interfaces is

   -----------------
   -- Next_Sample --
   -----------------

   function Next_Sample
     (Self : in out Generator) return Sample is
   begin
      return Generator'Class (Self).Next_Sample_Impl;
--        case Sample_Nb - Self.Current_Sample_Nb is
--           when 0 => null;
--           when 1 =>
--              Self.Current_Sample_Nb := Sample_Nb;
--              Self.Memo_Sample := Generator'Class (Self).Next_Sample_Impl;
--           when others =>
--              raise Constraint_Error
--                with "Cannot ask for a sample further in the future";
--        end case;
--
--        return Self.Memo_Sample;
   end Next_Sample;

   ------------------
   -- Next_Message --
   ------------------

   function Next_Message
     (Self : in out Note_Generator) return Note_Signal
   is
   begin
      case Sample_Nb - Self.Current_Sample_Nb is
         when 0 => null;
         when 1 =>
            Self.Current_Sample_Nb := Sample_Nb;
            Self.Memo_Signal := Note_Generator'Class (Self).Next_Message_Impl;
         when others =>
            raise Constraint_Error
              with "Cannot ask for a sample further in the future";
      end case;

      return Self.Memo_Signal;
   end Next_Message;

end Sound_Gen_Interfaces;
