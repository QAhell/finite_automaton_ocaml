open Finite_automaton
open Utf8_stream
open Simple_parser_combinator
open Trampoline
open Simple_json

let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;

module E =
struct
  type t = string
  let default_error = ""
  let is_default_error s = (s = "")
  let is_real_error s = (s <> "")
  let merge s t = s ^ "; " ^ t
  let make_error s _ = s
end

module A = Int_state_automaton (Code_point_input_of (Decoded_string_input)) (Code_point_output_of (Encoded_string_output))

module S = A.Serializer (Code_point_output_with_put_str (Encoded_string_output))
module P = Recursive_descent_parser (Trampoline) (E)
module J = Default_json_parser (P) (Decoded_string_input) (E)

let serialize_nfa a =
  Encoded_string_output.to_string (S.serialize_nfa (fun x -> x) (Encoded_string_output.empty ()) a) ;;
let serialize_dfa a =
  Encoded_string_output.to_string (S.serialize_dfa (fun x -> x) (Encoded_string_output.empty ()) a) ;;


let parse_json text =
  let input = Decoded_string_input.of_string text in
  let json = P.execute J.parse_json
      (fun output input _ ->
        match Decoded_string_input.get input with
          | None -> output
          | Some _ -> failwith ("Remaining input after parse!"))
      (fun e -> failwith e)
      input in
  json ;;

let test_serialize_nfa () =
  let start_state = 0 in
  let accepting_states = A.F.State.add 2 (A.F.State.singleton 1) in
  let transition =
        A.add_transition_nfa (42, 43) 0 1
        (A.add_transition_nfa (42, 42) 0 2 A.F.Transition.empty) in
  let a = { start_state = start_state ;
            accepting_states = accepting_states ;
            transition = transition } in
  let text = serialize_nfa a in
  let json = parse_json text in
  match json with
    | Object o ->
        assertTrue
          (String_map.find "start_state" o = Number (Positive, "0", "", None))
          "The serializer must correctly write the start state" ;
        (match String_map.find "accepting_states" o with
          | Array accepting_states ->
              assertTrue (
                  List.mem (Number (Positive, "1", "", None)) accepting_states &&
                  List.mem (Number (Positive, "2", "", None)) accepting_states &&
                  not (List.mem (Number (Positive, "0", "", None)) accepting_states))
                "The serializer must correctly write accepting states!"
          | _ -> failwith "The accepting_states must be a json array!") ;
        (match String_map.find "transition" o with
          | Array transitions ->
              assertTrue (2 = List.length transitions) "All two transition entries must be serialized!" ;
              let elt42 = List.find (function Array [Array [Number (Positive, "42", "", None);
                                                            Number (Positive, "42", "", None);
                                                            Number (Positive, "0", "", None)]; _] -> true
                                      | _ -> false) transitions in
              let elt43 = List.find (function Array [Array [Number (Positive, "43", "", None);
                                                            Number (Positive, "43", "", None);
                                                            Number (Positive, "0", "", None)]; _] -> true
                                      | _ -> false) transitions in
              (match elt42 with
                | Array [_; Array target] ->
                    assertTrue (2 = List.length target &&
                                List.mem (Number (Positive, "1", "", None)) target &&
                                List.mem (Number (Positive, "2", "", None)) target)
                      "All target states must be serialized for code point 42"
                | _ -> failwith "transition elements must be arrays") ;
              (match elt43 with
                | Array [_; Array target] ->
                    assertTrue (1 = List.length target &&
                                List.mem (Number (Positive, "1", "", None)) target)
                      "All target states must be serialized for code point 43"
                | _ -> failwith "transition elements must be arrays")
          | _ -> failwith "Transition must be a list!"
         )
    | _ -> failwith "Serialization must produce an object!" ;;

let test_serialize_dfa () =
  let start_state = 0 in
  let accepting_states = A.F.State.add 2 (A.F.State.singleton 1) in
  let transition =
        A.add_transition_dfa (43, 43) 0 1
        (A.add_transition_dfa (42, 42) 0 2 A.F.Transition.empty) in
  let a = { start_state = start_state ;
            accepting_states = accepting_states ;
            transition = transition } in
  let text = serialize_dfa a in
  let json = parse_json text in
  match json with
    | Object o ->
        assertTrue
          (String_map.find "start_state" o = Number (Positive, "0", "", None))
          "The serializer must correctly write the start state" ;
        (match String_map.find "accepting_states" o with
          | Array accepting_states ->
              assertTrue (
                  List.mem (Number (Positive, "1", "", None)) accepting_states &&
                  List.mem (Number (Positive, "2", "", None)) accepting_states &&
                  not (List.mem (Number (Positive, "0", "", None)) accepting_states))
                "The serializer must correctly write accepting states!"
          | _ -> failwith "The accepting_states must be a json array!") ;
        (match String_map.find "transition" o with
          | Array transitions ->
              assertTrue (2 = List.length transitions) "All two transition entries must be serialized!" ;
              let elt42 = List.find (function Array [Array [Number (Positive, "42", "", None);
                                                            Number (Positive, "42", "", None);
                                                            Number (Positive, "0", "", None)]; _] -> true
                                      | _ -> false) transitions in
              let elt43 = List.find (function Array [Array [Number (Positive, "43", "", None);
                                                            Number (Positive, "43", "", None);
                                                            Number (Positive, "0", "", None)]; _] -> true
                                      | _ -> false) transitions in
              (match elt42 with
                | Array [_; Number (Positive, "2", "", None)] -> ()
                | _ -> failwith "All target states must be serialized for code point 42") ;
              (match elt43 with
                | Array [_; Number (Positive, "1", "", None)] -> ()
                | _ -> failwith "All target states must be serialized for code point 43")
          | _ -> failwith "Transition must be a list!"
         )
    | _ -> failwith "Serialization must produce an object!" ;;



let test () =
  test_serialize_nfa () ;
  test_serialize_dfa () ;;

test () ;;
