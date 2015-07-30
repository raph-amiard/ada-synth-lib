with Utils; use Utils;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Waves; use Waves;

package Effects is

   ----------------
   -- Attenuator --
   ----------------

   type Attenuator is new Generator with record
      Level : Float;
      Source : access Generator'Class;
   end record;

   overriding procedure Next_Samples
     (Self : in out Attenuator);

   --------------------
   -- Dyn_Attenuator --
   --------------------

   type Dyn_Attenuator is new Generator with record
      Level_Provider : Generator_Access;
      Source : Generator_Access;
   end record;

   function VCA (Source : Generator_Access;
                 G : Generator_Access) return access Dyn_Attenuator
   is (new Dyn_Attenuator'(Source => Source,
                           Level_Provider => G, others => <>));

   overriding procedure Next_Samples
     (Self : in out Dyn_Attenuator);

   ----------------
   -- Transposer --
   ----------------

   type Transposer is new Generator with record
      Source : Generator_Access;
   end record;

   overriding procedure Next_Samples
     (Self : in out Transposer);

   -------------------
   -- Digital_Disto --
   -------------------

   type Digital_Disto is new Signal_Processor with record
      Clip_Level : Sample;
   end record;

   function Create_Digi_Dist
     (Clip_Level : Float) return access Digital_Disto;

   overriding function Process
     (Self : in out Digital_Disto; S : Sample) return Sample;

   -------------------
   -- Digital_Disto --
   -------------------

   type Disto is new Generator with record
      Clip_Level : Sample;
      Coeff : Sample := 10.0;
      Source : Generator_Access;
   end record;

   function Create_Dist
     (Source : access Generator'Class;
      Clip_Level : Float; Coeff : Float := 10.0) return access Disto;

   overriding procedure Next_Samples
     (Self : in out Disto);

   ---------------
   -- LP_Filter --
   ---------------

   type Low_Pass_Filter is new Generator with record
      Cut_Freq : Float;
      Cut_Freq_Provider : Generator_Access;
      Res : Float;
      A0, A1, A2, B1, B2 : Float;
      D1, D2, D3, D4 : Float := 0.0;
      Source : Generator_Access;
   end record;

   function Create_LP (Source : access Generator'Class;
                       Cut_Freq_Provider : access Generator'Class;
                       Q : Float) return access Low_Pass_Filter;

   overriding procedure Next_Samples
     (Self : in out Low_Pass_Filter);

   -----------
   -- Mixer --
   -----------

   type Mixer_Generator is record
      Gen : access Generator'Class;
      Level : Float;
   end record;

   type Generator_Vector is array (Natural range 0 .. 32) of Mixer_Generator;

   type Mixer is new Generator with record
      Generators : Generator_Vector;
      Length     : Natural := 0;
      Env        : access ADSR;
      Saturate   : Boolean := False;
   end record;

   type Generators_Arg_Array is array (Natural range <>) of Mixer_Generator;
   No_Generators : constant Generators_Arg_Array (1 .. 0) := (others => <>);

   function Create_Mixer
     (Sources : Generators_Arg_Array;
      Env : access ADSR := null;
      Saturate : Boolean := True) return access Mixer;

   function Add_Generator
     (Self : in out Mixer; G : access Generator'Class;
      Level : Float) return Natural;

   procedure Add_Generator
     (Self : in out Mixer; G : access Generator'Class; Level : Float);

   overriding procedure Next_Samples (Self : in out Mixer);

   type Delay_Line is new Generator with record
      Source           : access Generator'Class;
      Delay_In_Samples : B_Range_T;
      Decay            : Sample;
      Last_Buffer      : Generator_Buffer := (others => 0.0);
   end record;

   function Create_Delay_Line (Source : access Generator'Class;
                               Dlay : Millisecond;
                               Decay : Sample) return access Delay_Line;

   overriding procedure Next_Samples (Self : in out Delay_Line);

end Effects;
