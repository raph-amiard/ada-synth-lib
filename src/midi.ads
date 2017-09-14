with Interfaces; use Interfaces;

package MIDI is

   type Parser is tagged private;

   type I_Event_Listener is interface;
   procedure Note_On
     (Self     : in out I_Event_Listener;
      Channel  :        Unsigned_8;
      Note     :        Unsigned_8;
      Velocity :        Unsigned_8) is abstract;
   procedure Note_Off
     (Self     : in out I_Event_Listener;
      Channel  :        Unsigned_8;
      Note     :        Unsigned_8;
      Velocity :        Unsigned_8) is abstract;
   procedure Control_Change
     (Self              : in out I_Event_Listener;
      Channel           :        Unsigned_8;
      Controller_Number :        Unsigned_8;
      Controller_Value  :        Unsigned_8) is abstract;

   --  creates a new parser with a listener for receiving MIDI events
   function Create_Parser
     (Event_Listener : access I_Event_Listener'Class) return access Parser;

   --  parses one incoming byte and calls the listener for received MIDI events
   procedure Parse (Self : in out Parser; Received : Unsigned_8);

private
   procedure Test_Note_Command (Self : in out Parser; Received : Unsigned_8);

   type Parser is tagged record
      Event_Listener : access I_Event_Listener'Class;
      Wait_For_Event : Boolean;
      Last_Command   : Unsigned_8;
      Byte_Counter   : Unsigned_8;
      Byte0          : Unsigned_8;
      Byte1          : Unsigned_8;
   end record;

end MIDI;
