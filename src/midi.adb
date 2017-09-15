package body MIDI is

   procedure Test_Note_Command (Self : in out Parser; Received : Unsigned_8) is
   begin
      if ((Received and 16#80#) = 16#80#) or
        ((Received and 16#f0#) = 16#90#) or
        ((Received and 16#f0#) = 16#b0#)
      then
         Self.Wait_For_Event := False;
         Self.Byte_Counter   := 0;
         Self.Last_Command   := Received;
      else
         Self.Wait_For_Event := True;
      end if;
   end Test_Note_Command;

   function Create_Parser
     (Event_Listener : access I_Event_Listener'Class) return access Parser
   is
      Ret : constant access Parser :=
        new Parser'
          (Event_Listener => Event_Listener,
           Wait_For_Event => True,
           others         => 0);
   begin
      return Ret;
   end Create_Parser;

   procedure Parse (Self : in out Parser; Received : Unsigned_8) is
   begin
      --  ignore system real time messages
      if Received >= 16#f8# then
         return;
      end if;

      --  parse note-on or note-off message
      if Self.Wait_For_Event then
         Test_Note_Command (Self, Received);
      else
         --  if a command byte is received, test for note command
         if (Received and 16#80#) > 0 then
            Test_Note_Command (Self, Received);
            return;
         end if;

         --  otherwise read the next 2 bytes
         Self.Byte0        := Self.Byte1;
         Self.Byte1        := Received;
         Self.Byte_Counter := Self.Byte_Counter + 1;
         if Self.Byte_Counter = 2 then
            if (Self.Last_Command and 16#f0#) = 16#90# then
               --  test for note-on message
               if Self.Byte1 = 0 then
                  --  special case: note-on message with velocity 0 is used
                  --  as noteOff by some instruments
                  Self.Event_Listener.Note_Off
                  (Self.Last_Command and 16#f#, Self.Byte0, 0);
               else
                  Self.Event_Listener.Note_On
                  (Self.Last_Command and 16#f#, Self.Byte0, Self.Byte1);
               end if;
            elsif (Self.Last_Command and 16#f0#) = 16#80# then
               --  test for note-off message
               Self.Event_Listener.Note_Off
               (Self.Last_Command and 16#f#, Self.Byte0, Self.Byte1);
            elsif (Self.Last_Command and 16#f0#) = 16#b0# then
               --  test for control change message
               Self.Event_Listener.Control_Change
               (Self.Last_Command and 16#f#, Self.Byte0, Self.Byte1);
            end if;
            --  otherwise it is an unknown message and we ignore it

            --  reset byte counter only, not the waitForNoteCommand flag,
            --  to handle "running status"
            Self.Byte_Counter := 0;
         end if;
      end if;
   end Parse;

end MIDI;
