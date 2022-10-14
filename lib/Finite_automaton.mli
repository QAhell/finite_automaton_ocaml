type ('result, 'error) result =
  | Result of 'result
  | Error of 'error

(** Input for finite automaton. *)
module type Input =
sig
  include Interval_map.Ordered_incrementable_type

  (** An input sequence of ordered incrementable values. *)
  type ts

  val get : ts -> (t * ts) option
end

(** Input with upper and lower bounds on the symbol set. *)
module type Bounded_input =
sig
  include Input
  val minimum : t
  val maximum : t
end

(** Option_input lifted to input modules. *)
module Option_input : functor (I : Input) -> Input with type t = I.t option and type ts = I.ts

(** Converts Utf8_stream.Code_point_input into Input. *)
module Code_point_input_of (I : Utf8_stream.Code_point_input) :
  Bounded_input with type t = int and type ts = I.t

(** Converts Utf8_stream.Byte_input into Input. *)
module Byte_input_of (I : Utf8_stream.Byte_input) :
  Input with type t = char and type ts = I.t

(** Converts sequence modules into input for finite automaton. *)
module Input_of_sequence :
  functor (S : Simple_sequence.Sequence_definition) ->
    functor (O : Interval_map.Ordered_incrementable_type) ->
      Input with type t = O.t and type ts = O.t S.t


(** Output of finite automaton. *)
module type Output =
sig
  type o
  type os
  val put : os -> o -> os
end

(** Converts Utf8_stream.Code_point_output into Output. *)
module Code_point_output_of (O : Utf8_stream.Code_point_output) :
  Output with type o = int and type os = O.t

(** Converts Utf8_stream.Byte_output into Output. *)
module Byte_output_of (O : Utf8_stream.Byte_output) :
  Output with type o = char and type os = O.t

(** Converts sequence modules into output of finite automaton.
  The automaton uses put=cons, so the output sequence will be
  reversed. *)
module Output_of_sequence :
  functor (S : Simple_sequence.Sequence_definition) ->
    functor (O : Interval_map.Ordered_type) ->
      Output with type o = O.t and type os = O.t S.t

(** Sets of states. *)
module type State =
  sig
    include Set.S

    (** States can be compared. FIXME: maybe this can be removed. *)
    val compare_elt : elt -> elt -> int
  end

(** An ordering on the elements of a set. *)
module Element_ord :
  functor (S : State) -> Interval_map.Ordered_type with type t = S.elt

(** See [Set.Make]. *)
module State_make :
  functor (O : Interval_map.Ordered_type) ->
    State with type elt = O.t and
               type t = Set.Make (O).t

(** The type of transition tables. It's just like an interval map. *)
module Transition_type :
  functor (I : Interval_map.Ordered_incrementable_type) ->
    functor (S : State) ->
      sig
        module type T =
          sig
            (** The type of the transition table. *)
            type 'a t

            (** An empty transition table. *)
            val empty : 'a t

            (** Returns [true] if there is an entry with
               the given input symbol and state. *)
            val mem : I.t -> S.elt -> 'a t -> bool

            (** Returns the entry in the table for the
               given input symbol and state. *)
            val find : I.t -> S.elt -> 'a t -> 'a

            (** Puts a new entry into the table,
               DON'T USE THIS DIRECTLY, this is just the map interface,
               it's not a multi-map!  Since non-deterministic finite automaton
               store multiple transitions in one table entry, you should add a
               new transition with a custom add_transition function that finds
               the set, adds one element to the set and adds the set back to
               the map. *)
            val add : ('a -> 'a -> bool) ->
              (I.t * I.t) -> S.elt -> 'a -> 'a t -> 'a t

            (** Updates entries in a certain interval and
               adds new entries if there's no value. *)
            val update : ('a -> 'a -> bool) ->
              (I.t * I.t) -> S.elt -> ('a -> 'a) -> 'a -> 'a t -> 'a t

            (** [values (l, r) s t] returns all states that
               can be reaced from [s] via a symbol from the
               range [(l, r)]. *)
            val values : I.t * I.t -> S.elt -> 'a t -> 'a list

            (** Accumulates a value while iterating through the
               whole transition table. *)
            val fold :
              ((I.t * I.t) -> S.elt -> 'a -> 'b -> 'b) ->
                'a t -> 'b -> 'b
          end
      end

(** The record type of an automaton. *)
type ('a, 'b, 'c) automaton =
  { start_state : 'a ; (** The initial state of the automaton. *)
    accepting_states : 'b ; (** All accepting final states. *)
    transition : 'c (** The transition table. *) }

module type Finite_automaton =
sig
  module Input : Input
  module State : State
  module Transition : Transition_type(Input)(State).T

  (** Sets containing input symbols. *)
  module Input_symbol_set : Interval_map.Interval_set_type with type elt = Input.t

  (** [inputs_of s a] returns all input symbols
     of all transitions of [a] beginning in [s]. *)
  val inputs_of :
    State.t -> ('a, 'b, 'c Transition.t) automaton -> unit Interval_map.Array_interval_map(Input).t

  (** [range_of_nfa i a s] returns all states [t]
     that have a transition from [s] via
     input symbol [i] to [t]. *)
  val range_of_nfa : Input.t * Input.t -> ('a, 'b, State.t Transition.t) automaton ->
                State.elt -> State.t

  (** [reachable_nfa i a s acc] accumulates states
     that are reachable from [s] using only the
     input symbol [i]. *)
  val reachable_nfa :
    Input.t -> ('a, 'b, State.t Transition.t) automaton ->
      State.elt -> State.t -> State.t

  (** Returns a set of all input symbols of an automaton. *)
  val alphabet_of : ('a, 'b, 'c Transition.t) automaton -> Input_symbol_set.t

  val equivalent_intervals :
    ('a, 'b, 'c Transition.t) automaton ->
      unit Interval_map.Array_interval_map(Input).t

  val reachable_states_of_nfa
    : (State.elt, State.t, State.t Transition.t) automaton -> State.t
  val reachable_states_of_dfa
    : (State.elt, State.t, State.elt Transition.t) automaton -> State.t

  (** Returns all states of a non-deterministic finite
   automaton. *)
  val states_of_nfa : (State.elt, State.t, State.t Transition.t) automaton -> State.t

  (** Returns all states of a deterministic
     finite automaton. *)
  val states_of_dfa : (State.elt, State.t, State.elt Transition.t) automaton -> State.t

  module Output : Output with type o = Input.t
  module Int_t :
    Transition_type(Input)(State_make (Interval_map.Int_ord)).T

  (** Executes a non-deterministic finite automaton without empty
     (i.e. epsilon-) transitions. The longest prefix with the
     remaining input is returned. If there is no match, None is
     returned. *)
  val execute_nfa : ('a, State.t, State.t Transition.t) automaton
                  -> State.t -> Input.ts -> Output.os -> Input.ts option
                  -> (Output.os * Input.ts) option

  (** Executes a deterministic finite automaton without empty
     (i.e. epsilon-) transitions. The longest prefix with the
     remaining input is returned.
     If there is no match, it returns [Error].
   *)
  val execute_dfa : ('a, State.t, State.elt Transition.t) automaton
                  -> State.elt -> Input.ts -> Output.os -> (Output.os * State.elt * Input.ts) option
                  -> (Output.os * State.elt * Input.ts, Input.ts) result

  (** Returns a minimized deterministic finite automaton. *)
  val minimize_dfa :
        (State.elt, State.t, State.elt Transition.t) automaton ->
        (State.elt, State.t, State.elt Transition.t) automaton
end

(** A convenience functor to create transitions. *)
module Transition_make :
  functor (I : Interval_map.Ordered_incrementable_type) ->
    functor (S : State) ->
      Transition_type(I)(S).T

(** This is the type of the module Finite_automaton. *)
module Finite_automaton_type :
  functor (I : Input) ->
    functor (S : State) ->
      functor (O : Output with type o = I.t) ->
        sig
          module type T = Finite_automaton
            with module Input = I
             and module State = S
             and module Output = O
             and module Transition = Transition_make (I) (S)
             and type 'a Transition.t = 'a Transition_make (I) (S).t
        end

(** Useful operations on a single finite automaton. *)
module Finite_automaton :
  functor (I : Input) ->
    functor (S : State) ->
      functor (O : Output with type o = I.t) ->
        Finite_automaton_type(I)(S)(O).T
          with module Input = I
           and module State = S
           and module Output = O
           and module Transition = Transition_make (I) (S)
           and type 'a Transition.t = 'a Transition_make (I) (S).t

(** This module contains the conversion from non-deterministic finite automaton
   with empty (i.e. epsilon-) transitions to non-deterministic finite automaton
   without them. *)
module Nfa_of_enfa :
  functor (To : Finite_automaton) ->
    sig
      (** Converts a non-deterministic finite automaton with
         epsilon-transitions to a non-deterministic finite automaton without
         them. *)
      val nfa_of_enfa :
            (To.State.elt, To.State.t, To.State.t Transition_make (Interval_map.Option_ord (To.Input)) (To.State).t) automaton ->
              (To.State.elt, To.State.t, To.State.t To.Transition.t) automaton
    end

(** This module contains functions that map the states of an automaton to
   integers. *)
module Int_nfa_of_nfa :
  functor (From : Finite_automaton) ->
  functor (To : Finite_automaton with
    module Input = From.Input and
    module State = State_make (Interval_map.Int_ord) and
    module Output = From.Output) ->
          sig
            module M : Map.S with type key = From.State.elt

            (** Converts a dfa to an equivalent dfa where the states are ints.
              TODO: return a map from old states to new states.
              *)
            val int_dfa_of_dfa :
                      (M.key, From.State.t, M.key From.Transition.t) automaton ->
                      int ->
                      (State_make (Interval_map.Int_ord).elt, State_make (Interval_map.Int_ord).t,
                        State_make (Interval_map.Int_ord).elt To.Transition.t) automaton * int

            (** Converts a nfa to an equivalent nfa where the states are ints. *)
            val int_nfa_of_nfa :
                      (M.key, From.State.t, From.State.t From.Transition.t) automaton ->
                      int ->
                      (State_make (Interval_map.Int_ord).elt, State_make (Interval_map.Int_ord).t,
                        State_make (Interval_map.Int_ord).t To.Transition.t) automaton * int
          end

(** This module contains the conversion from non-deterministic finite automaton
   without empty (i.e. epsilon-) transitions to deterministic finite automaton. *)
module Dfa_of_nfa :
  functor (From : Finite_automaton) ->
    functor (To : Finite_automaton with
      module State = State_make (From.State) and
      module Input = From.Input and
      module Output = From.Output) ->
        sig
          (** Returns a deterministic finite automaton given a non-deterministic
             finite automaton. This function performs the usual power set
             construction. *)
          val dfa_of_nfa :
                (From.State.elt, From.State.t, From.State.t From.Transition.t) automaton ->
                (From.State.t, To.State.t, To.State.elt To.Transition.t) automaton
        end

module Product_automaton :
  functor (Left : Finite_automaton) ->
    functor (Right : Finite_automaton with
                        module Input = Left.Input and
                        module Output = Left.Output) ->
      sig
        module S : State with type elt = Left.State.elt * Right.State.elt
        module F : Finite_automaton_type (Left.Input) (S) (Left.Output).T
        val conjunction_nfa :
          (Left.State.elt, Left.State.t,
            Left.State.t Left.Transition.t) automaton ->
          (Right.State.elt, Right.State.t,
            Right.State.t Right.Transition.t) automaton ->
          (F.State.elt, F.State.t,
            F.State.t F.Transition.t) automaton
      end

module Complement_automaton :
  functor (B : Bounded_input) ->
    functor (F : Finite_automaton with module Input = B) ->
        sig
          val negate_dfa :
            (F.State.elt, F.State.t, F.State.elt F.Transition.t) automaton ->
            F.State.elt ->
            (F.State.elt, F.State.t, F.State.elt F.Transition.t) automaton
        end

module Int_set : State with type elt = int and type t = State_make (Interval_map.Int_ord).t


(** Finite automaton with states of type int. *)
module Int_state_automaton_type :
  functor (I : Bounded_input) ->
    functor (O : Output with type o = I.t) ->
      sig
        module type T =
          sig
            module F : Finite_automaton_type (I) (Int_set) (O).T
            (*module E : Finite_automaton_type (Option_input (I)) (Int_set) (O).T*)
            module EI : Input with type t = I.t option
            module ET : Transition_type(EI)(Int_set).T

            (* These abbreviations should probably be somewhere else. *)
            type nfa = (int, F.State.t, F.State.t F.Transition.t) automaton
            type dfa = (int, F.State.t, int F.Transition.t) automaton
            type enfa = (int, F.State.t, F.State.t ET.t) automaton

            val nfa_of_enfa : enfa -> int -> nfa * int
            val dfa_of_nfa : nfa -> int -> dfa * int
            val enfa_of_dfa : dfa -> enfa
            val enfa_of_nfa : nfa -> enfa
            val nfa_of_dfa : dfa -> nfa
            val conjunction_nfa : nfa -> nfa -> int -> nfa * int
            val negation_dfa : dfa -> int -> dfa
            val minimize_dfa : dfa -> dfa

            (* These functions should probably be somewhere else. *)
            val add_transition_dfa : I.t * I.t -> int -> int -> int F.Transition.t -> int F.Transition.t
            val add_transition_nfa : I.t * I.t -> int -> int -> Int_set.t F.Transition.t -> Int_set.t F.Transition.t
            val add_transition_enfa : (I.t option * I.t option) -> int -> int -> Int_set.t ET.t -> Int_set.t ET.t
          end
      end

(** Finite automaton with states of type int. *)
module Int_state_automaton :
  functor (I : Bounded_input) ->
    functor (O : Output with type o = I.t) ->
      Int_state_automaton_type (I) (O).T


