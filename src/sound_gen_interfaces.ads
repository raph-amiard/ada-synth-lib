with Utils; use Utils;
with Array_Utils;

package Sound_Gen_Interfaces is

   Sample_Nb : Sample_Period := 0;
   --  Global state for ASL: This is the state of the sound simulation, eg. at
   --  which sample we are right now. TODO: Put this in a context, rather than
   --  in a global.

   -------------
   -- Buffers --
   -------------

   Generator_Buffer_Length : constant := 512;

   type Buffer_Range_Type is range 0 .. Generator_Buffer_Length - 1;
   type Generator_Buffer is array (Buffer_Range_Type) of Sample;
   --  Type for a generator buffer, used as work areas for generators.

   ---------------
   -- Generator --
   ---------------

   type Generator;
   --  Base type for a generator. A generator is the basic building block to
   --  create synth graphs in Ada synth lib. By creating a tree of generators,
   --  the user can create complex instruments.

   type Generator_Access is access all Generator'Class;

   package Generator_Arrays
   is new Array_Utils (Generator_Access);
   subtype Generator_Array is Generator_Arrays.Array_Type;
   subtype Generator_Vector is Generator_Arrays.Vectors.Vector;
   Empty_Generator_Array : Generator_Array := Generator_Arrays.Empty_Array;
   --  Utility types to handle collection of generators

   ----------------------
   --  Parameter scope --
   ---------------------------

   type Params_Scope_Type is record
      Generators : Generator_Arrays.Vector_Type;
   end record;
   type Params_Scope is access all Params_Scope_Type;

   procedure Enter (F : Params_Scope);
   procedure Leave (F : Params_Scope);
   procedure Add_To_Current (G : Generator_Access);

   type Generator is abstract tagged record
      Parameters : Params_Scope;
   end record;

   procedure Next_Samples
     (Self : in out Generator; Buffer : in out Generator_Buffer) is abstract;
   pragma Inline (Next_Samples);

   procedure Base_Reset (Self : in out Generator);
   procedure Reset_Not_Null (Self : Generator_Access);

   procedure Reset (Self : in out Generator) is abstract;

   function Children
     (Self : in out Generator) return Generator_Array is abstract;

   function All_Children
     (Self : in out Generator) return Generator_Array;

   function Has_Params_Scope
     (Self : in out Generator) return Boolean is (False);

   function Is_Param
     (Self : in out Generator) return Boolean is (False);
   procedure Compute_Params (Self : in out Generator);

   function Get_Params
     (Self : in out Generator) return Generator_Arrays.Array_Type;

   function Nb_Values (Self : in out Generator) return Natural is (0);
   procedure Set_Value
     (Self : in out Generator; I : Natural; Val : Float) is null;

   function Get_Value
     (Self : in out Generator; Dummy : Natural) return Float is (0.0);
   function Get_Name
     (Self : in out Generator; Dummy : Natural) return String is ("");
   function Get_Min_Value
     (Self : in out Generator; Dummy : Natural) return Float is (0.0);
   function Get_Max_Value
     (Self : in out Generator; Dummy : Natural) return Float is (0.0);

   type Scaled_Value_T is new Float range 0.0 .. 1.0;

   procedure Set_Scaled_Value
     (Self : in out Generator'Class; I : Natural; Val : Scaled_Value_T);

   type Param_Scale_T is (Linear, Exp);

   function Get_Scale
     (Self : in out Generator; Dummy : Natural) return Param_Scale_T
   is (Linear);

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

   type I_Simulation_Listener is interface;
   procedure Next_Step (Self : in out I_Simulation_Listener) is abstract;
   function Name
     (Self : in out I_Simulation_Listener) return String is abstract;

   type Note_Signal_Buffer is array (Buffer_Range_Type) of Note_Signal;

   type Note_Generator is abstract tagged record
      Buffer            : Note_Signal_Buffer;
   end record;
   type Note_Generator_Access is access all Note_Generator'Class;

   procedure Reset (Self : in out Note_Generator) is abstract;
   procedure Reset_Not_Null (Self : Note_Generator_Access);

   type Sim_Listener_Array is
     array (Natural range <>) of access I_Simulation_Listener'Class;

   Simulation_Listeners    : Sim_Listener_Array (0 .. 1024);
   Simulation_Listeners_Nb : Natural := 0;

   procedure Register_Simulation_Listener
     (N : access I_Simulation_Listener'Class);
   procedure Next_Steps;

   type Sequencer_Note is record
      Note     : Note_T;
      Duration : Sample_Period;
   end record;

   No_Seq_Note : Sequencer_Note := (Note => No_Note, Duration => 0);

end Sound_Gen_Interfaces;
