with Ada.Unchecked_Conversion;
with System;
with Utils;       use Utils;
with Ada.Text_IO; use Ada.Text_IO;
with Config;      use Config;

package body Soundio_Output is

   Initial_Drift_Level : int := 0;
   Current_Drift_Level : int := 0;

   function Get_Next_Sample
     (G : User_Data_Access) return Float;

   function Access_To_Address is new Ada.Unchecked_Conversion
     (Source => User_Data_Access, Target => System.Address);

   function Get_User_Data
     (Out_Stream : access SoundIo_Out_Stream) return User_Data_Access;

   ---------------------
   -- Set_Ring_Buffer --
   ---------------------

   procedure Set_Ring_Buffer
     (Out_Stream : access SoundIo_Out_Stream;
      Ring_Buf   : FRB.Ring_Buffer;
      G          : access Generator'Class)
   is
   begin
      Out_Stream.User_Data := Access_To_Address
        (new Soundio_User_Data'
           (Mode => Buffer,
            G    => G,
            G_Buffer => <>,
            Ring_Buf => Ring_Buf,
            Current_Sample => <>, S => Stop));
   end Set_Ring_Buffer;

   -------------------
   -- Set_Generator --
   -------------------

   procedure Set_Generator (Out_Stream : access SoundIo_Out_Stream;
                            G          : access Generator'Class)
   is
   begin
      Out_Stream.User_Data := Access_To_Address
        (new Soundio_User_Data'
           (Mode           => Callback, G => G, G_Buffer => <>,
            Current_Sample => <>, S => Stop));
   end Set_Generator;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Out_Stream : access SoundIo_Out_Stream) return User_Data_Access
   is
      pragma Warnings (Off, "possible aliasing");
      function Address_To_Access is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => User_Data_Access);
      pragma Warnings (On, "possible aliasing");
   begin
      return Address_To_Access (Out_Stream.User_Data);
   end Get_User_Data;

   -------------------
   -- Get_Generator --
   -------------------

   function Get_Generator
     (Out_Stream : access SoundIo_Out_Stream) return User_Data_Access
   is
   begin
      return Get_User_Data (Out_Stream);
   end Get_Generator;

   ---------------------
   -- Get_Next_Sample --
   ---------------------

   function Get_Next_Sample
     (G : User_Data_Access) return Float
   is
   begin
      if G.Current_Sample = Buffer_Range_Type'Last then
         Next_Steps;
         G.G.Next_Samples (G.G_Buffer);
         Sample_Nb := Sample_Nb + Generator_Buffer_Length;
         G.Current_Sample := 0;
      else
         G.Current_Sample := G.Current_Sample + 1;
      end if;

      return Float (G.G_Buffer (G.Current_Sample));
   end Get_Next_Sample;

   Dummy_Err            : SoundIo_Error;

   procedure Write_Float_Sample is new Write_Sample (Float);

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int)
   is
      Layout            : SoundIo_Channel_Layout renames Out_Stream.Layout;
      Areas             : SoundIo_Channel_Area_Ptr;
      Frames_Left       : int := Frame_Count_Max;
      Sample            : Float;
      use Soundio_Channel_Area_Ptrs;
      User_Data         : constant User_Data_Access
        := Get_User_Data (Out_Stream);
      Available_Frames  : Natural;
   begin
      if Initial_Drift_Level = 0 then
         Initial_Drift_Level := Frame_Count_Max - (Frame_Count_Max / 10);
      else
         Current_Drift_Level := Frame_Count_Max;
      end if;

      if Config.Debug then
         Put_Line
           ("In the write callback, " & Frame_Count_Min'Img
            & " " & Frame_Count_Max'Img);
      end if;

      if User_Data.Mode = Callback then

         while Frames_Left > 0 loop
            declare
               Frame_Count : int := Frames_Left;
            begin
               Dummy_Err :=
                 Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);
               exit when Frame_Count = 0;

               if User_Data.S = Play then
                  for Frame in 0 .. Frame_Count - 1 loop
                     Sample := Get_Next_Sample (Get_Generator (Out_Stream));
                     for Channel in 0 .. Layout.Channel_Count - 1 loop
                        Write_Float_Sample
                          (Get_Area (Areas, Channel), Frame, Sample);
                     end loop;
                  end loop;
               else
                  for Frame in 0 .. Frame_Count - 1 loop
                     for Channel in 0 .. Layout.Channel_Count - 1 loop
                        Write_Float_Sample
                          (Get_Area (Areas, Channel), Frame, 0.0);
                     end loop;
                  end loop;
               end if;

               Dummy_Err := Outstream_End_Write (Out_Stream);
               Frames_Left := Frames_Left - Frame_Count;
            end;
         end loop;

      else

         Available_Frames := FRB.Available_Read_Frames (User_Data.Ring_Buf);

         if Config.Debug then
            Put_Line ("Available frames to read : " & Available_Frames'Img);
         end if;

         Frames_Left := int'Min (Frames_Left, int (Available_Frames));

         while Frames_Left > 0 loop
            declare
               FC : int := Frames_Left;
            begin
               Dummy_Err :=
                 Outstream_Begin_Write (Out_Stream, Areas, FC);

               exit when FC = 0;

               for Frame in 0 .. FC - 1 loop
                  Sample := FRB.Read (User_Data.Ring_Buf);
                  for Channel in 0 .. Layout.Channel_Count - 1 loop
                     Write_Float_Sample
                       (Get_Area (Areas, Channel), Frame, Sample);
                  end loop;

                  Frames_Left := Frames_Left - 1;
               end loop;

               Dummy_Err := Outstream_End_Write (Out_Stream);
            end;
         end loop;

         if Config.Debug then
            Put_Line ("Frames left : " & Frames_Left'Img);
            Put_Line ("Left frames to read : "
                      & FRB.Available_Read_Frames (User_Data.Ring_Buf)'Img);
         end if;

      end if;
   end Write_Callback;

   -------------------
   -- Write_Samples --
   -------------------

   procedure Write_Samples
     (Out_Stream : access SoundIo_Out_Stream;
      Max_Nb_Samples : Natural := Natural (Buffer_Range_Type'Last))
   is
      U : constant User_Data_Access := Get_User_Data (Out_Stream);
      Available_Frames : Natural := FRB.Available_Write_Frames (U.Ring_Buf);
      Written_Samples  : Natural := 0;
   begin
      if U.S = Stop then
         for Dummy in 0 .. Max_Nb_Samples - 1 loop
            exit when Available_Frames = 0;
            FRB.Write (U.Ring_Buf, 0.0);
            Available_Frames := Available_Frames - 1;
            Written_Samples := Written_Samples + 1;
         end loop;
      else
         for Dummy in 0 .. Max_Nb_Samples - 1 loop
            exit when Available_Frames = 0;
            FRB.Write (U.Ring_Buf, Get_Next_Sample (U));
            Available_Frames := Available_Frames - 1;
            Written_Samples := Written_Samples + 1;
         end loop;
      end if;
   end Write_Samples;

   ----------
   -- Play --
   ----------

   procedure Play
     (Out_Stream : access SoundIo_Out_Stream)
   is
      U : constant User_Data_Access := Get_User_Data (Out_Stream);
   begin
      if U.S = Play then
         return;
      end if;
      U.S := Play;
   end Play;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Out_Stream : access SoundIo_Out_Stream)
   is
      U : constant User_Data_Access := Get_User_Data (Out_Stream);
   begin

      if U.S = Stop then
         return;
      end if;

      U.S := Stop;
      Sample_Nb := 0;
      U.G.Reset;
   end Stop;

   -----------------
   -- Drift_Level --
   -----------------

   function Drift_Level return int
   is
     (int'Max (Current_Drift_Level - Initial_Drift_Level, 0));

end Soundio_Output;
