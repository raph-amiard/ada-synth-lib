with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

package Effects is

   ----------------
   -- Attenuator --
   ----------------

   type Attenuator is new Generator with record
      Level : Float;
      Source : Generator_Access;
   end record;

   overriding function Is_Param (Self : in out Attenuator) return Boolean
   is (True);

   overriding function Nb_Values
     (Self : in out Attenuator) return Natural is (1);

   overriding procedure Set_Value
     (Self : in out Attenuator; I : Natural; Val : Float);

   overriding function Get_Value
     (Self : in out Attenuator; Dummy : Natural) return Float
   is (Self.Level);

   overriding function Get_Name
     (Self : in out Attenuator; Dummy : Natural) return String
   is
     ("Attenuator");

   overriding function Get_Min_Value
     (Self : in out Attenuator; Dummy : Natural) return Float is (0.0);

   overriding function Get_Max_Value
     (Self : in out Attenuator; Dummy : Natural) return Float is (5_000.0);

   overriding procedure Next_Samples
     (Self : in out Attenuator; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Attenuator);

   overriding function Children
     (Self : in out Attenuator) return Generator_Array is ((0 => Self.Source));

   --------------------
   -- Dyn_Attenuator --
   --------------------

   type Dyn_Attenuator is new Generator with record
      Level_Provider : Generator_Access;
      Source         : Generator_Access;
   end record;

   function VCA (Source : Generator_Access;
                 G : Generator_Access) return Generator_Access
   is (new Dyn_Attenuator'(Source => Source,
                           Level_Provider => G, others => <>));

   overriding procedure Next_Samples
     (Self : in out Dyn_Attenuator; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Dyn_Attenuator);

   overriding function Children
     (Self : in out Dyn_Attenuator) return Generator_Array
   is ((Self.Level_Provider, Self.Source));

   ----------------
   -- Transposer --
   ----------------

   type Transposer is new Generator with record
      Source : Generator_Access;
   end record;

   overriding procedure Next_Samples
     (Self : in out Transposer; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Transposer);

   overriding function Children
     (Self : in out Transposer) return Generator_Array
   is ((0 => Self.Source));

   -----------
   -- Disto --
   -----------

   type Disto is new Generator with record
      Clip_Level : Sample;
      Coeff : Sample := 10.0;
      Source : Generator_Access;
   end record;

   function Create_Dist
     (Source : access Generator'Class;
      Clip_Level : Float; Coeff : Float := 10.0) return Generator_Access;

   overriding procedure Next_Samples
     (Self : in out Disto; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Disto);

   overriding function Children
     (Self : in out Disto) return Generator_Array
   is ((0 => Self.Source));

   ---------------
   -- LP_Filter --
   ---------------

   type Low_Pass_Filter is new Generator with record
      Cut_Freq_Provider  : Generator_Access;
      Source             : Generator_Access;
      Res                : Float;
      Cut_Freq           : Float;
      A0, A1, A2, B1, B2 : Float;
      D1, D2, D3, D4     : Float := 0.0;
   end record;

   --  TODO: Why Res/Q should probably have a provider.

--     overriding function Has_Params_Scope
--       (Self : in out Low_Pass_Filter) return Boolean is (True);

   function Create_LP
     (Source   : Generator_Access;
      Cut_Freq : Generator_Access;
      Q        : Float) return Generator_Access;

   overriding function Is_Param (Self : in out Low_Pass_Filter) return Boolean
   is (True);

   overriding procedure Next_Samples
     (Self : in out Low_Pass_Filter; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Low_Pass_Filter);

   overriding function Children
     (Self : in out Low_Pass_Filter) return Generator_Array
   is ((Self.Cut_Freq_Provider, Self.Source));

   overriding function Nb_Values
     (Self : in out Low_Pass_Filter) return Natural is (1);

   overriding procedure Set_Value
     (Self : in out Low_Pass_Filter; I : Natural; Val : Float);

   overriding function Get_Value
     (Self : in out Low_Pass_Filter; Dummy : Natural) return Float
   is (Self.Res);

   overriding function Get_Name
     (Self : in out Low_Pass_Filter; Dummy : Natural) return String
   is
     ("Q");

   overriding function Get_Min_Value
     (Self : in out Low_Pass_Filter; Dummy : Natural) return Float is (0.0);

   overriding function Get_Max_Value
     (Self : in out Low_Pass_Filter; Dummy : Natural) return Float is (1.0);

   -----------
   -- Mixer --
   -----------

   type Track is record
      Gen   : access Generator'Class;
      Level : Float;
   end record;

   type Generator_Vector is array (Natural range 0 .. 32) of Track;

   type Mixer is new Generator with record
      Generators  : Generator_Vector;
      Length      : Natural := 0;
      Env         : Generator_Access;
      Saturate    : Boolean := False;
      Work_Buffer : Generator_Buffer;
   end record;
   type Mixer_Access is access all Mixer;

   type Generators_Arg_Array is array (Natural range <>) of Track;
   No_Generators : constant Generators_Arg_Array (1 .. 0) := (others => <>);

   function Create_Mixer
     (Sources    : Generators_Arg_Array;
      Volume_Mod : Generator_Access := null;
      Saturate   : Boolean := True) return Mixer_Access;
   --  Create a mixer. Will clip if ``Saturate`` is ``True``. If ``Volume_Mod``
   --  is passed it will be used to modulate the output volume of the mixer.

   function Create_Mixer
     (Sources    : Generators_Arg_Array;
      Volume_Mod : Generator_Access := null;
      Saturate   : Boolean := True) return Generator_Access
   is (Generator_Access
         (Mixer_Access'(Create_Mixer (Sources, Volume_Mod, Saturate))));

   function Add_Generator
     (Self  : in out Mixer;
      G     : access Generator'Class;
      Level : Float) return Natural;
   --  Add a new generator to the mixer, with level ``Level``. Return the index
   --  of the generator in the mixer.

   procedure Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float);
   --  Add a new generator to the mixer, with level ``Level``.

   overriding procedure Next_Samples
     (Self : in out Mixer; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Mixer);

   overriding function Children
     (Self : in out Mixer) return Generator_Array;

   -----------------
   --  Delay line --
   -----------------

   type Delay_Line is new Generator with record
      Source           : access Generator'Class;
      Delay_In_Samples : Buffer_Range_Type;
      Decay            : Sample;
      Last_Buffer      : Generator_Buffer := (others => 0.0);
   end record;

   function Create_Delay_Line (Source : access Generator'Class;
                               Dlay : Millisecond;
                               Decay : Sample) return Generator_Access;

   overriding procedure Next_Samples
     (Self : in out Delay_Line; Buffer : in out Generator_Buffer);

   overriding procedure Reset (Self : in out Delay_Line);

   overriding function Children
     (Self : in out Delay_Line) return Generator_Array
   is
     (0 => Self.Source);

end Effects;
