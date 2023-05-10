open Simple_sequence
open Interval_map

type ('result, 'error) result =
  | Result of 'result
  | Error of 'error

let id x = x
let id_if' b f x = if b then x else f x
let (<%) f g x = f (g x)
let eq_of_cmp compare x y =
  0 = compare x y

module type Input =
sig
  include Ordered_incrementable_type
  type ts
  val get : ts -> (t * ts) option
end

module type Bounded_input =
sig
  include Input
  val minimum : t
  val maximum : t
end

module Option_input (I : Input) =
struct
  include Option_ord (I)
  type ts = I.ts
  let get xs =
    match I.get xs with
      | Some (y, ys) -> Some (Some y, ys)
      | None -> None
end

module Code_point_input_of (I : Utf8_stream.Code_point_input) :
  Bounded_input with type t = int and type ts = I.t =
struct
  include Int_ord
  type ts = I.t
  let get = I.get
  let minimum = 0
  let maximum = 0x1FFFFF
end

module Byte_input_of (I : Utf8_stream.Byte_input) :
  Bounded_input with type t = char and type ts = I.t =
struct
  include Char_ord
  type ts = I.t
  let get = I.get
  let minimum = '\x00'
  let maximum = '\xFF'
end

module Input_of_sequence :
  functor (S : Sequence_definition) ->
    functor (O : Ordered_incrementable_type) ->
      Input with type t = O.t and type ts = O.t S.t =
  functor (S : Sequence_definition) ->
    functor (O : Ordered_incrementable_type) ->
      struct
        include O
        let compare = O.compare
        type ts = O.t S.t
        let get = S.get
      end

module type Output =
sig
  type o
  type os
  val put : os -> o -> os
end

module Code_point_output_of (O : Utf8_stream.Code_point_output) :
  Output with type o = int and type os = O.t =
struct
  type o = int
  type os = O.t
  let put = O.put
end

module Byte_output_of (O : Utf8_stream.Byte_output) :
  Output with type o = char and type os = O.t =
struct
  type o = char
  type os = O.t
  let put = O.put
end

module Output_of_sequence :
  functor (S : Sequence_definition) ->
    functor (O : Ordered_type) ->
      Output with type o = O.t and type os = O.t S.t =
   functor (S : Sequence_definition) ->
    functor (O : Ordered_type) ->
      struct
        type o = O.t
        type os = O.t S.t
        let put xs x =
          S.cons (fun () -> (x, xs))
      end

module type State =
sig
  include Set.S
  val compare_elt : elt -> elt -> int
end

module type Ordered_type =
sig
  type t
  val compare : t -> t -> int
end

let lex_compare cmp0 cmp1 (x0, x1) (y0, y1) =
  let d0 = cmp0 x0 y0 in
  if d0 <> 0 then d0
  else cmp1 x1 y1

module Unordered_pair_ord :
    functor (P : Ordered_type) ->
        Ordered_type with type t = P.t * P.t =
  functor (P : Ordered_type) ->
    struct
      type t= P.t * P.t
      let compare (x0, x1) (y0, y1) =
        let (x0, x1) = if P.compare x0 x1 > 0 then (x1, x0) else (x0, x1) in
        let (y0, y1) = if P.compare y0 y1 > 0 then (y1, y0) else (y0, y1) in
        lex_compare P.compare P.compare (x0, x1) (y0, y1)
    end

module Element_ord (S : State) : Ordered_type with type t = S.elt =
struct
  type t = S.elt
  let compare = S.compare_elt
end

module Lexicographic_ord :
  functor (P : Ordered_type) ->
    functor (Q : Ordered_type) ->
      Ordered_type with type t = P.t * Q.t =
  functor (P : Ordered_type) ->
    functor (Q : Ordered_type) ->
        struct
          type t = P.t * Q.t
          let compare (x0, x1) (y0, y1) =
            lex_compare Q.compare P.compare (x1, x0) (y1, y0)
        end

module State_make :
  functor (O : Ordered_type) ->
    State with type elt = O.t and
               type t = Set.Make (O).t =
  functor (O : Ordered_type) ->
    struct
      include Set.Make (O)
      let compare_elt = O.compare
    end

module Int_set = State_make (Int_ord)

module Transition_type =
  functor (I : Ordered_incrementable_type) ->
    functor (S : State) ->
      struct
        module type T =
          sig
            type 'a t
            val empty : 'a t
            val mem : I.t -> S.elt -> 'a t -> bool
            val find : I.t -> S.elt -> 'a t -> 'a
            (* don't use add! always use add_transition
               because 'a could be a set and you might
               just want to add an edge and not delete other
               edges. *)
            val add : ('a -> 'a -> bool) ->
              (I.t * I.t) -> S.elt -> 'a -> 'a t -> 'a t
            val update : ('a -> 'a -> bool) ->
              (I.t * I.t) -> S.elt -> ('a -> 'a) -> 'a -> 'a t -> 'a t
            val values : I.t * I.t -> S.elt -> 'a t -> 'a list
            val fold :
              ((I.t * I.t) -> S.elt -> 'a -> 'b -> 'b) ->
                'a t -> 'b -> 'b
          end
      end

type ('a, 'b, 'c) automaton =
  { start_state : 'a ;
    accepting_states : 'b ;
    transition : 'c }

(** The type of a module containing a few very basic functions on automaton. *)
module type Getter =
sig
  module Input : Input
  module State : State
  module Transition : Transition_type(Input)(State).T

  (** Sets containing input symbols. *)
  module Input_symbol_set : Interval_set_type with type elt = Input.t

  (** [inputs_of s a] returns all input symbols
     of all transitions of [a] beginning in [s]. *)
  val inputs_of :
    State.t -> ('a, 'b, 'c Transition.t) automaton -> unit Array_interval_map(Input).t

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
      unit Array_interval_map(Input).t

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
end

module Getter_type =
  functor (I : Input) ->
    functor (S : State) ->
      functor (T : Transition_type(I)(S).T) ->
        struct
          module type T =
            Getter with
              module Input = I and module State = S and module Transition = T
        end

module Getter : 
  functor (I : Input) ->
    functor (S : State) ->
      functor (T : Transition_type(I)(S).T) ->
        Getter_type (I) (S) (T).T =
  functor (I : Input) (S : State) (T : Transition_type(I)(S).T) ->
struct
  module Input = I
  module State = S
  module Transition = T
  module Input_symbol_set : Interval_set_type with type elt = I.t =
    Interval_set (I)
  module Imap = Array_interval_map (Input)
  let inputs_of states automaton =
    T.fold (fun i st _ ->
        id_if' (not (S.mem st states)) (Imap.update (fun _ _ -> false) i id ()))
      automaton.transition Imap.empty
  let range_of_nfa inp automaton state =
    List.fold_left S.union S.empty (T.values inp state automaton.transition)
  let rec reachable_nfa inp automaton state acc =
    if S.mem state acc then acc
    else
      S.fold (reachable_nfa inp automaton)
        (range_of_nfa (inp, inp) automaton state)
        (S.add state acc)
  let alphabet_of automaton =
    T.fold (fun i _ _ -> Input_symbol_set.add i)
      automaton.transition Input_symbol_set.empty
  let equivalent_intervals automaton =
    T.fold (fun (l, r) _ _ ->
        Imap.update (fun _ _ -> false) (l, r) id ()
        )
      automaton.transition Imap.empty
  let reachable_states_of_nfa nfa =
    let alphabet = equivalent_intervals nfa in
    let rec reachable_states state acc =
      if State.mem state acc then acc
      else
        let new_states =
          Imap.fold (fun (l, _) _ ->
            if T.mem l state nfa.transition then
              S.union (T.find l state nfa.transition)
            else
              id) alphabet S.empty in
        S.fold reachable_states new_states (S.add state acc) in
    reachable_states nfa.start_state S.empty
  let reachable_states_of_dfa dfa =
    let alphabet = equivalent_intervals dfa in
    let rec reachable_states state acc =
      if State.mem state acc then acc
      else
        let new_states =
          Imap.fold (fun (l, _) _ ->
            if T.mem l state dfa.transition then
              S.add (T.find l state dfa.transition)
            else
              id) alphabet S.empty in
        S.fold reachable_states new_states (S.add state acc) in
    reachable_states dfa.start_state S.empty
  let states_of_nfa automaton =
    T.fold (fun _ st sts -> S.add st <% S.union sts)
      automaton.transition (S.add automaton.start_state
                              automaton.accepting_states)
  let states_of_dfa automaton =
    T.fold (fun _ st sts -> S.add st <% S.add sts)
      automaton.transition (S.add automaton.start_state
                              automaton.accepting_states)
end

(** Provides map functions for dfa and nfa states. *)
module Map_automaton (B0 : Getter)
    (B1 : Getter with module Input = B0.Input) =
struct
  module S0 = B0.State
  module S1 = B1.State
  module T0 = B0.Transition
  module T1 = B1.Transition
  let map_dfa eq_v f nfa =
    { start_state = f nfa.start_state ;
      accepting_states =
        S0.fold (S1.add <% f) nfa.accepting_states S1.empty ;
      transition =
        T0.fold (fun i s s' -> T1.add eq_v i (f s) (f s'))
          nfa.transition T1.empty }
  (*let map_nfa eq_v f nfa =
    { start_state = f nfa.start_state ;
      accepting_states =
        S0.fold (S1.add <% f) nfa.accepting_states S1.empty ;
      transition =
        T0.fold (fun i s s' ->
            T1.add eq_v i (f s) (S0.fold (S1.add <% f) s' S1.empty))
         nfa.transition T1.empty }*)
end

module type Finite_automaton =
sig
  module Input : Input
  module State : State
  module Transition : Transition_type(Input)(State).T
  module Input_symbol_set : Interval_set_type with type elt = Input.t
  val inputs_of :
    State.t -> ('a, 'b, 'c Transition.t) automaton -> unit Array_interval_map(Input).t
  val range_of_nfa : Input.t * Input.t -> ('a, 'b, State.t Transition.t) automaton ->
                State.elt -> State.t
  val reachable_nfa :
    Input.t -> ('a, 'b, State.t Transition.t) automaton ->
      State.elt -> State.t -> State.t
  val alphabet_of : ('a, 'b, 'c Transition.t) automaton -> Input_symbol_set.t
  val equivalent_intervals :
    ('a, 'b, 'c Transition.t) automaton ->
      unit Array_interval_map(Input).t
  val reachable_states_of_nfa
    : (State.elt, State.t, State.t Transition.t) automaton -> State.t
  val reachable_states_of_dfa
    : (State.elt, State.t, State.elt Transition.t) automaton -> State.t
  val states_of_nfa : (State.elt, State.t, State.t Transition.t) automaton -> State.t
  val states_of_dfa : (State.elt, State.t, State.elt Transition.t) automaton -> State.t
  module Output : Output with type o = Input.t
  module Int_t :
    Transition_type(Input)(State_make (Int_ord)).T
  val execute_nfa : ('a, State.t, State.t Transition.t) automaton
                  -> State.t -> Input.ts -> Output.os -> Input.ts option
                  -> (Output.os * Input.ts) option
  val execute_dfa : ('a, State.t, State.elt Transition.t) automaton
                  -> State.elt -> Input.ts -> Output.os -> (Output.os * State.elt * Input.ts) option
                  -> (Output.os * State.elt * Input.ts, Input.ts) result
  val minimize_dfa :
        (State.elt, State.t, State.elt Transition.t) automaton ->
        (State.elt, State.t, State.elt Transition.t) automaton
end

module Transition_make :
    functor (I : Ordered_incrementable_type) ->
      functor (S : State) ->
        Transition_type(I)(S).T =
  functor (I : Ordered_incrementable_type) ->
    functor (S : State) ->
      struct
        module L : Ordered_incrementable_type
          with type t = I.t * S.elt =
            Lexicographic_incrementable_ord (I) (Element_ord (S))
        module M : Interval_map_type (L).T
            with type key = (I.t * S.elt) * (I.t * S.elt) =
          Interval_map (L)

        type 'a t = 'a M.t
        let empty = M.empty
        let mem i s m =
          M.mem (i, s) m
        let find i s m =
          M.find (i, s) m
        let add eq_v (i0, i1) s v m =
          M.add eq_v ((i0, s), (i1, s)) v m
        let update eq_v (i0, i1) s f v m =
          M.update eq_v ((i0, s), (i1, s)) f v m
        let values (i0, i1) s m =
          M.values ((i0, s), (i1, s)) m
        let fold f m acc =
          M.fold (fun ((i0, s), (i1, _)) -> f (i0, i1) s) m acc
      end

module Finite_automaton_type =
  functor (I : Input) ->
      functor (S : State) ->
        functor (O : Output with type o = I.t) ->
          struct
            module type T =
              Finite_automaton with module Input = I and module State = S and module Output = O and module Transition = Transition_make (I) (S)
          end

module Finite_automaton (I : Input) (S : State) (O : Output with type o = I.t) =
struct
  module Output = O
  module T = Transition_make (I) (S)
  module B = Getter (I) (S) (T)
  include B
  let rec execute_nfa automaton current_states input acc result =
    let return () =
      match result with
        | Some res -> Some (acc, res)
        | None -> None in
    match I.get input with
      | Some (inp, input) ->
          let new_states =
            S.fold (fun state acc ->
                if Transition.mem inp state
                    automaton.transition then
                  S.union (Transition.find inp state
                                  automaton.transition) acc
                else acc) current_states S.empty in
          if S.is_empty new_states then
            return ()
          else
          begin
            let new_result =
              if not (S.is_empty (S.inter new_states automaton.accepting_states)) then
                Some input
              else result in
            execute_nfa automaton new_states input (O.put acc inp) new_result
          end
      | None -> return ()
  let rec execute_dfa automaton current_state input acc result =
    let return () =
      match result with
        | Some (acc, state, res) -> Result (acc, state, res)
        | None -> Error input in
    match I.get input with
      | Some (inp, input) ->
          if Transition.mem inp current_state automaton.transition then
            let new_state = Transition.find inp current_state automaton.transition in
            let new_acc = O.put acc inp in
            let new_result =
              if S.mem new_state automaton.accepting_states then
                Some (new_acc, new_state, input)
              else result in
           execute_dfa automaton new_state input new_acc new_result
          else
            return ()
      | None -> return ()

  module P = Set.Make (Unordered_pair_ord (Element_ord (S)))
  module Map_s = Map.Make (S)
  module S_map = Map_automaton (B) (B)
  module M = Map.Make (Element_ord (S))

  let rec find x tbl =
    if not (M.mem x tbl) then
      (x, M.add x x tbl)
    else
      if 0 = S.compare_elt x (M.find x tbl) then
        (M.find x tbl, tbl)
      else
        let y, tbl = find (M.find x tbl) tbl in
        (y, M.add x y tbl)

  (* module Imap = Interval_map (I)*)
  module Imap = Array_interval_map(B.Input)
  let flip f x y = f y x
  module Iset : Set.S with type elt = I.t = Set.Make (I)

  let minimize_dfa dfa =
    let alphabet = equivalent_intervals dfa in
    let states = states_of_dfa dfa in
    let state_pairs = (* cross product of unordered pairs *)
      S.fold (fun s ->
         S.fold (fun s' ->
            P.add (s, s')) states) states P.empty in
    let rec remove_unequivalent pairs = (* recursion steps: same symbol mustn't lead to uneq states *)
      let (is_changed, pairs) =
        P.fold (fun (s, s') ->
          Imap.fold (fun interval () (is_changed, pairs) ->
            if Transition.mem (fst interval) s dfa.transition =
               Transition.mem (fst interval) s' dfa.transition then
              if Transition.mem (fst interval) s dfa.transition then
                if P.mem (Transition.find (fst interval) s dfa.transition,
                          Transition.find (fst interval) s' dfa.transition) pairs then
                  (is_changed, pairs)
                else
                  (true, P.remove (s, s') pairs)
              else
                (is_changed, pairs)
            else
              (true, P.remove (s, s') pairs)) alphabet)
          pairs (false, pairs) in
      if is_changed then
        remove_unequivalent pairs
      else pairs in
    let equivalent_states =
      state_pairs
        |> P.fold (fun (s, t) -> (* base case: accepting states uneq to non-accepting states *)
            if S.mem s dfa.accepting_states <> S.mem t dfa.accepting_states then
              P.remove (s, t)
            else id) state_pairs
        |> remove_unequivalent in
    let eqtbl = M.empty in
    let eqtbl = P.fold (fun (s, t) eqtbl -> (* make eqtbl a union-find structure *)
        let s', eqtbl = find s eqtbl in
        let t', eqtbl = find t eqtbl in
        if 0 = S.compare_elt s' t' then eqtbl
        else M.add t' s' eqtbl) equivalent_states eqtbl in
    S_map.map_dfa (fun x y -> 0 = S.compare_elt x y) (flip M.find eqtbl) dfa

  module Int_t = Transition_make (I) (State_make (Int_ord))
  module D_map = Map_automaton (B) (Getter  (I) (State_make (Int_ord)) (Int_t))
end

module Enfa_type =
  functor (I : Input) ->
    functor (S : State) ->
      struct
        module type T =
          sig
            include Getter_type(Option_input(I))(S)(Transition_make(Option_ord(I))(S)).T
              with module Input = Option_input (I)
                and module State = S
                and module Transition = Transition_make(Option_ord(I))(S)
          end
      end

module Enfa (I : Input) (S : State) : Enfa_type(I)(S).T =
struct
  module Enfa = Getter (Option_input(I)) (S) (Transition_make (Option_ord(I))(S))
  include Enfa
end

module Nfa_of_enfa
  (To : Finite_automaton)
  (*From : Enfa_type (To.Input) (To.State).T*) =
struct
  module From = Enfa (To.Input) (To.State)
  module S = To.State

  module Js = Array_interval_map(From.Input)

  let nfa_of_enfa enfa =
    let alphabet = From.equivalent_intervals enfa in
    let states = From.states_of_nfa enfa in
    let rec eps_neighbors p (visited, res) =
      if S.mem p visited then (visited, res)
      else
        let neighbors0 =
          if From.Transition.mem None p enfa.transition then
            From.Transition.find None p enfa.transition
          else S.empty in
        S.fold eps_neighbors neighbors0
          (S.add p visited, S.union neighbors0 res) in
    let eps_ns p =
      snd (eps_neighbors p (S.empty, S.empty)) in
    (* mk_transition: if p -via symbol c and epsilons-> m then add p -c-> m *)
    let mk_transition p c m res =
      To.Transition.update (eq_of_cmp S.compare) c p (S.add m) S.empty res in
    (* mk_transition_c: if p -via epsilons-> n -via symbol c and epsilons-> ms
        then add p -c-> m for every m in ms *)
    let mk_transition_c p n c res =
      let (l, _) = c in
      let ms0 =
        if From.Transition.mem (Some l) n enfa.transition then
          From.Transition.find (Some l) n enfa.transition
        else S.empty in
      let ms = S.union ms0 (S.fold (S.union <% eps_ns) ms0 S.empty) in
        S.fold (mk_transition p c) ms res in
    (* mk_transitions_n: like mk_transition_c but for every symbol *)
    let mk_transitions_n p n res =
      Js.fold (function
          | None, None -> (fun () -> id)
          | Some _, None | None, Some _ ->
              (fun () -> failwith "nfa_of_enfa: Invalid enfa alphabet!")
          | Some inp, Some inp' -> (fun () ->
              mk_transition_c p n (inp, inp'))) alphabet res in
    (* mk_transitions: like mk_transitions_n but for every epsilon-neighbor *)
    let mk_transitions p res =
      S.fold (mk_transitions_n p)
        (S.add p (eps_ns p)) res in
    let accepted = enfa.accepting_states in
    let accepted' =
      if S.fold (fun st -> (||) (S.mem st accepted)) (eps_ns enfa.start_state) false then
        S.add enfa.start_state accepted
      else accepted in
    { start_state = enfa.start_state ;
      accepting_states = accepted' ;
      transition = S.fold mk_transitions states To.Transition.empty }
end

module Int_nfa_of_nfa
  (From : Finite_automaton)
  (To : Finite_automaton with
    module Input = From.Input and
    module State = State_make (Int_ord) and
    module Output = From.Output) =
struct
  module I = From.Input
  module S = From.State
  module O = From.Output

  module M = Map.Make (
      struct
        type t = S.elt
        let compare = S.compare_elt
      end)

  let int_dfa_of_dfa automaton max_state_so_far =
    let m : int M.t ref = ref M.empty in
    let fresh_ref = ref max_state_so_far in
    let fresh_state () = incr fresh_ref ; !fresh_ref in
    let f (st : S.elt) =
      if M.mem st !m then
        M.find st !m
      else
       (m := M.add st (fresh_state ()) !m ;
        M.find st !m) in
    let add_transition sym from to_ t =
      To.Transition.add (fun x y -> 0 = x - y) sym (f from) (f to_) t in
    let t =
      From.Transition.fold add_transition automaton.transition To.Transition.empty in
    ({ start_state = f automaton.start_state ;
      accepting_states =
        S.fold (fun x -> Int_set.add (f x)) automaton.accepting_states Int_set.empty ;
      transition = (t : int To.Transition.t) }, !fresh_ref)

  let int_nfa_of_nfa automaton max_state_so_far =
    let m : int M.t ref = ref M.empty in
    let fresh_ref = ref max_state_so_far in
    let fresh_state () = incr fresh_ref ; !fresh_ref in
    let f (st : S.elt) =
      if M.mem st !m then
        M.find st !m
      else
       (m := M.add st (fresh_state ()) !m ;
        M.find st !m) in
    let add_transition sym from to_ t =
      To.Transition.update (eq_of_cmp Int_set.compare)
        sym (f from) (S.fold (Int_set.add <% f) to_)
          Int_set.empty t in
    let t =
      From.Transition.fold add_transition automaton.transition To.Transition.empty in
    ({ start_state = f automaton.start_state ;
      accepting_states =
        S.fold (fun x -> Int_set.add (f x)) automaton.accepting_states Int_set.empty ;
      transition = (t : Int_set.t To.Transition.t) }, !fresh_ref)
end

module Dfa_of_nfa
  (From : Finite_automaton)
  (To : Finite_automaton with
    module State = State_make (From.State) and
    module Input = From.Input and
    module Output = From.Output) =
struct
  module I = From.Input
  module S = From.State
  module O = From.Output
  module Imap = Array_interval_map (From.Input)
  module S2 = To.State
  module Js = Array_interval_map(I)
  let dfa_of_nfa nfa =
    let start_state =
      S.singleton nfa.start_state in
    let transitions_for old_states t =
      Imap.fold (fun symbol () (t, s2) ->
          let value =
            S.fold (S.union <% From.range_of_nfa symbol nfa)
              old_states S.empty in
          (To.Transition.add (eq_of_cmp S.compare) symbol old_states value t,
           S2.add value s2))
        (From.inputs_of old_states nfa) (t, S2.empty) in
    let rec all_transitions new_state (acc, transition) =
      if acc |> S2.mem new_state then
        (acc, transition)
      else
        let (transition, states) = transitions_for new_state transition in
        S2.fold all_transitions states (acc |> S2.add new_state, transition) in
    let dfa0 =
      { start_state = start_state ;
        accepting_states = S2.empty ;
        transition = snd (all_transitions start_state
                              (S2.empty, To.Transition.empty)) } in
    let accepting_states =
      S2.filter (fun s ->
          not (S.is_empty (S.inter s
              nfa.accepting_states)))
        (To.states_of_dfa dfa0) in
    { start_state = start_state ;
      accepting_states = accepting_states ;
      transition = dfa0.transition }
end

module Product_automaton
    (Left : Finite_automaton)
    (Right : Finite_automaton with
                module Input = Left.Input and
                module Output = Left.Output) =
struct
  module S =
    State_make (Lexicographic_ord
                  (Element_ord (Left.State))
                  (Element_ord (Right.State)))
  module F = Finite_automaton (Left.Input) (S) (Left.Output)
  module Imap = Array_interval_map (Left.Input)
  module Imap' = Array_interval_map (Right.Input)
  let state_product left_set right_set =
    Left.State.fold (fun l ->
        Right.State.fold (fun r ->
          S.add (l, r)) right_set)
      left_set S.empty

  let common_equivalent_intervals left_automaton right_automaton =
    Imap'.fold (fun i _ ->
          Imap.update (fun _ _ -> false) i id ())
      (Right.equivalent_intervals right_automaton)
      (Left.equivalent_intervals left_automaton)

  let conjunction_nfa left_nfa right_nfa =
    let alphabet = common_equivalent_intervals left_nfa right_nfa in
    let add_transition sym from to_ t =
      F.Transition.update (eq_of_cmp S.compare) sym from (S.add to_) S.empty t in
    let rec put_transitions ((from_left, from_right) as from) (states, transition) =
      if S.mem from states then
        (states, transition)
      else
        Imap.fold (fun ((sym0, _) as sym) _ (states, transition) ->
            if Left.Transition.mem sym0 from_left left_nfa.transition &&
                Right.Transition.mem sym0 from_right right_nfa.transition then
              let to_states = state_product
                (Left.Transition.find sym0 from_left left_nfa.transition)
                (Right.Transition.find sym0 from_right right_nfa.transition) in
              S.fold put_transitions to_states
                (states, S.fold (add_transition sym from) to_states transition)
            else (states, transition)) alphabet (S.add from states, transition) in
    {
      start_state = (left_nfa.start_state, right_nfa.start_state) ;
      accepting_states = state_product (left_nfa.accepting_states) (right_nfa.accepting_states) ;
      transition = snd (put_transitions (left_nfa.start_state,
        right_nfa.start_state) (S.empty, F.Transition.empty)) ;
    }
end

module Complement_automaton (B : Bounded_input)
  (F : Finite_automaton with
        type Input.t = B.t
        and type Input.ts = B.ts) =
struct
  let complete_dfa_transitions automaton dummy_state =
    let states_of_automaton = F.states_of_dfa automaton in
    { automaton with
        transition = automaton.transition
                      |> (states_of_automaton |> F.State.fold (fun state ->
                          F.Transition.update (eq_of_cmp F.State.compare_elt)
                            (B.minimum, B.maximum) state id dummy_state))
                      |> F.Transition.add (eq_of_cmp F.State.compare_elt)
                          (B.minimum, B.maximum) dummy_state dummy_state }
  let negate_dfa automaton dummy_state =
    let automaton = complete_dfa_transitions automaton dummy_state in
    let all_states = F.states_of_dfa automaton in
    { automaton with
        accepting_states =
          F.State.diff all_states automaton.accepting_states }
end

module Int_state_automaton_type =
  functor (I : Bounded_input) ->
    functor (O : Output with type o = I.t) ->
      struct
        module type T =
          sig
            module F : Finite_automaton_type (I) (Int_set) (O).T
            (*module E : Enfa_type (F.Input) (F.State).T*)
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

            module Simple_error :
              functor (Input : sig include Utf8_stream.Code_point_input
                                val line : t -> int
                                val column : t -> int
                                val position : t -> int end) ->
              sig
                include Simple_parser_combinator.Error_info with type t = string Utf8_stream.with_position
                val make_error : string -> Input.t -> t
              end

            module Serializer :
              functor (Output : sig include Utf8_stream.Code_point_output val put_str : t -> string -> t end) ->
                sig
                  val serialize_nfa : (I.t -> int) -> Output.t -> nfa -> Output.t
                  val serialize_dfa : (I.t -> int) -> Output.t -> dfa -> Output.t
                end

          end
      end

module Int_state_automaton (I : Bounded_input) (O : Output with type o = I.t) =
struct
  module F =
    struct
      include Finite_automaton (I) (Int_set) (O)
    end
  module E2F = Nfa_of_enfa (F)
  module E = E2F.From
  module EI = E.Input
  module ET = E.Transition
  module D = Finite_automaton (F.Input) (State_make (F.State)) (F.Output)
  module F2D = Dfa_of_nfa (F) (D)
  module Prod = Product_automaton (F) (F)
  module Comp = Complement_automaton (I) (F)

  module D2F = Int_nfa_of_nfa (D) (F)
  module Prod2F = Int_nfa_of_nfa (Prod.F) (F)

  include F

  type nfa = (int, F.State.t, F.State.t F.Transition.t) automaton
  type dfa = (int, F.State.t, int F.Transition.t) automaton
  type enfa = (int, E.State.t, E.State.t E.Transition.t) automaton


  let nfa_of_enfa a i = E2F.nfa_of_enfa a, i
  let dfa_of_nfa = D2F.int_dfa_of_dfa <% F2D.dfa_of_nfa
  let conjunction_nfa l r =
    Prod2F.int_nfa_of_nfa (Prod.conjunction_nfa l r)
  let negation_dfa = Comp.negate_dfa

  let add_transition_dfa c from to_ t =
    F.Transition.add (eq_of_cmp (-)) c from to_ t
  let add_transition_nfa c from to_ t =
    F.Transition.update (eq_of_cmp F.State.compare)
      c from (F.State.add to_) F.State.empty t
  let add_transition_enfa c from to_ t =
    E2F.From.Transition.update (eq_of_cmp E2F.From.State.compare)
      (*match c with Some (l, r) -> (Some l, Some r) | None -> (None, None)*) c from (E2F.From.State.add to_) E2F.From.State.empty t
  let enfa_of_dfa automaton =
    { automaton with
        transition =
          F.Transition.fold (fun (l, r) state to_ ->
              add_transition_enfa ((Some l, Some r)) state to_)
            automaton.transition E2F.From.Transition.empty }
  let enfa_of_nfa nfa =
    { nfa with
        transition =
          F.Transition.fold (fun (l, r) state to_ ->
            F.State.fold (add_transition_enfa (Some l, Some r) state)
              to_) nfa.transition E2F.From.Transition.empty
    }
  let nfa_of_dfa automaton =
    { automaton with
        transition =
          F.Transition.fold (fun (l, r) state to_ ->
              add_transition_nfa (l, r) state to_)
            automaton.transition F.Transition.empty }

  module Simple_error (Input : sig include Utf8_stream.Code_point_input
                                val line : t -> int
                                val column : t -> int
                                val position : t -> int end) =
    struct
      type t = string Utf8_stream.with_position
      let default_error = {
          Utf8_stream.input = "<not an error>" ;
          Utf8_stream.line = -1 ;
          Utf8_stream.column = -1 ;
          Utf8_stream.position = -1 ;
        }
      let is_default_error e =
        e.Utf8_stream.input = "<not an error>" &&
        e.Utf8_stream.line = -1 &&
        e.Utf8_stream.column = -1 &&
        e.Utf8_stream.position = -1
      let is_real_error e = not (is_default_error e)
      let merge e1 e2 = (* more specific error wins, i. e. longer parser wins *)
        if e1.Utf8_stream.position >= e2.Utf8_stream.position then e1 else e2
      let make_error msg i = {
          Utf8_stream.input = msg ;
          Utf8_stream.line = Input.line i ;
          Utf8_stream.column = Input.column i ;
          Utf8_stream.position = Input.position i ;
        }
    end

  module Serializer (Output : sig include Utf8_stream.Code_point_output val put_str : t -> string -> t end) =
    struct
      open Simple_json
      module P = Json_printer (Output)
      let (|>) x f = f x
      let json_number_of_int n =
        Number (((if n >= 0 then Positive else Negative),
                string_of_int (abs n), "", None))
      let json_list_of_nfa_transition l r from to_ =
        Array ([
            Array (List.map json_number_of_int [l; r; from]);
            Array (Int_set.elements to_
                    |> List.map json_number_of_int);
        ])
      let json_of_nfa int_of_input_elt nfa =
        Object (
          String_map.empty
            |> String_map.add "start_state" (
                json_number_of_int nfa.start_state)
            |> String_map.add "accepting_states" (Array (
                nfa.accepting_states
                  |> Int_set.elements
                  |> List.map json_number_of_int))
            |> String_map.add "transition" (Array ([] |> (
                nfa.transition
                  |> F.Transition.fold (fun (l, r) from to_ acc ->
                        json_list_of_nfa_transition (int_of_input_elt l) (int_of_input_elt r) from to_ :: acc)))))
      let serialize_nfa int_of_input_elt output nfa =
        P.print_json output (json_of_nfa int_of_input_elt nfa)

      let json_list_of_dfa_transition l r from to_ =
        Array ([
            Array (List.map json_number_of_int [l; r; from]);
            json_number_of_int to_;
        ])
      let json_of_dfa int_of_input_elt dfa =
        Object (
          String_map.empty
            |> String_map.add "start_state" (
                json_number_of_int dfa.start_state)
            |> String_map.add "accepting_states" (Array (
                dfa.accepting_states
                  |> Int_set.elements
                  |> List.map json_number_of_int))
            |> String_map.add "transition" (Array ([] |> (
                dfa.transition |>
                  F.Transition.fold (fun (l, r) from to_ acc ->
                      json_list_of_dfa_transition (int_of_input_elt l) (int_of_input_elt r) from to_ :: acc)))))
      let serialize_dfa int_of_input_elt output dfa =
        P.print_json output (json_of_dfa int_of_input_elt dfa)
    end
end
