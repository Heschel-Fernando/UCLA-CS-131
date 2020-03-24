(* type: symbol definition (in HW1 / HW2) *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* type: parse_tree definition *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* 
 * 1. To warm up, notice that the format of grammars is different in this 
 * assignment, versus Homework 1. Write a function convert_grammar gram1 
 * that returns a Homework 2-style grammar, which is converted from the 
 * Homework 1-style grammar gram1. Test your implementation of convert_grammar 
 * on the test grammars given in Homework 1. For example, the top-level 
 * definition let awksub_grammar_2 = convert_grammar awksub_grammar should bind
 * awksub_grammar_2 to a Homework 2-style grammar that is equivalent to the 
 * Homework 1-style grammar awksub_grammar. 
 *)  
let convert_grammar gram1 =
  let start_symbol, rules = gram1 in 
  (* 
   * helper function that takes hw1 rules and return production function,
   * which takes nonterminal_symbol as argument.
   *)
  let rec convert_grammar_helper rules nonterminal_symbol = 
    match rules with
    | [] -> []
    | head::tail ->
      let symbol, right_hand_side = head in
      if symbol = nonterminal_symbol
      then right_hand_side::(convert_grammar_helper tail nonterminal_symbol)
      else convert_grammar_helper tail nonterminal_symbol
  in
  (* currying *)
  start_symbol, convert_grammar_helper rules


(* 
 * 2. As another warmup, write a function parse_tree_leaves tree that traverses 
 * the parse tree tree left to right and yields a list of the leaves 
 * encountered.
 *)
let rec parse_tree_leaves tree = 

  (* 
   * helper function of parse_tree_leaves 
   * traverse tree node in dfs fashion
   *)
  let rec dfs tree = 
    match tree with
    | Node (nonterminal, children) -> dfs_helper children
    | Leaf terminal -> [terminal]
  (* 
   * helper function of dfs
   * traverse children from left to right
   *)
  and dfs_helper children =
    match children with
    | [] -> []
    | head::tail -> dfs head @ dfs_helper tail
  in
  
  dfs tree
  

(* 
 * 3. Write a function make_matcher gram that returns a matcher for the grammar
 * gram. When applied to an acceptor accept and a fragment frag, the matcher 
 * must try the grammar rules in order and return the result of calling accept 
 * on the suffix corresponding to the first acceptable matching prefix of frag; 
 * this is not necessarily the shortest or the longest acceptable match. A match
 * is considered to be acceptable if accept succeeds when given the suffix 
 * fragment that immediately follows the matching prefix. When this happens, the
 * matcher returns whatever the acceptor returned. If no acceptable match is 
 * found, the matcher returns None.
 * 
 * make_matcher takes grammar as argument and return a matcher for this grammar,
 * which takes acceptor and fragment as arguments. A matcher must 
 * check all possible prefix and feed it into acceptor (one at a time).
 * If acceptor accepts it, matcher should returns whatever acceptor returns.
 * If acceptor rejects all prefix, then matcher returns None.
 *)
let make_matcher gram = fun accept frag ->
  (* hw2 grammar's second element is function instead of list *)
  let start_symbol, production_function = gram in
  (* 
   * alternative_list is right hand side of rules 
   * we need to consume all alternatives before returning None
   *)
  let alternative_list = production_function start_symbol in 

  (* 
   * helper function for make_matcher 
   * iterate_alternative iterates over an alternative_list
   * For each alternative, it checks if acceptor accepts it.
   * If it does, return whatever it returns.
   * Otherwise, check next alternative.
   *)
  let rec iterate_alternative alternative_list accept frag =
    match alternative_list with
    (* Consumed all alternative_list. Need to backtrack *)
    | [] -> None
    (* Explore alternatives *)
    | alternative_list_head::alternative_list_tail -> 
      let acceptable_match = 
        iterate_right_hand_side alternative_list_head accept frag 
      in
      match acceptable_match with
      (* alternative_list_head is accepted by acceptor *)
      | Some x -> Some x
      (* alternative_list_head is not accepted. Explore next rule *)
      | None -> iterate_alternative alternative_list_tail accept frag
  (*
   * helper function for make_matcher
   * iterate_right_hand_side iterates over right_hand_side
   * For each right_hand_side, it resolves the nonterminal to a terminal.
   * If the terminal matches the fragment, it returns whatever acceptor
   * returns for resulting suffix. Otherwise, it returns None.
   *)
  and iterate_right_hand_side right_hand_side accept frag =
    match right_hand_side, frag with
    (* 
     * Return whatever acceptor returns.
     * Note this accept function here will propagate the result
     * from right most token of an alternative to left most token
     * of an alternative.
     * e.g. <Term> <Binop> <Expr> <suffix>
     * At <Expr>, it returns whatever acceptor returns 
     * At <Binop>, it returns whatever we returned at <Expr>
     * At <Term>, it returns whatever we returned at <Binop>
     *)
    | [], _ -> accept frag
    (* Number of tokens do not match. Need to backtrack. *)
    | _, [] -> None
    (* check if right_hand_side_head matches frag *)
    | right_hand_side_head::right_hand_side_tail, frag_head::frag_tail ->
      match right_hand_side_head with
      (* no need to resolve symbol further (i.e. reached leaf node ) *)
      | T terminal ->
        if terminal = frag_head
        (* Terminal matches. Continue iterate right_hand_side *)
        then iterate_right_hand_side right_hand_side_tail accept frag_tail
        (* Terminal does not match. Need to backtrack. *)
        else None
      (* still need to resolve the symbol *)
      | N nonterminal ->
        (* 
         * eventually when nonterminal resolves to a terminal that matches,
         * all right_hand_side_tail must be accepted for current
         * right_hand_side to be accepted
         *)
        iterate_alternative 
          (* new alternative_list *)
          (production_function nonterminal) 
          (* new acceptor *)
          (iterate_right_hand_side right_hand_side_tail accept) 
          frag
  in
  iterate_alternative alternative_list accept frag


(* 
 * 4. Write a function make_parser gram that returns a parser for the grammar 
 * gram. When applied to a fragment frag, the parser returns an optional parse 
 * tree. If frag cannot be parsed entirely (that is, from beginning to end), 
 * the parser returns None. Otherwise, it returns Some tree where tree is the 
 * parse tree corresponding to the input fragment. Your parser should try 
 * grammar rules in the same order as make_matcher. 
 *)
let make_parser gram = fun frag ->

  (* 
   * helper function for make_parser
   * Given a grammar, return a function that takes an argument frag.
   * This function (return value of make_parse_tracker) should return the parse
   * tree generated given frag. If not possible, return None.
   * make_parse_tracker is a slight modification of make_matcher.
   * Just added a extra code to keep track of traces of rules applied.
   * Other than that, everything else is the same.
   * traces is a list of (nonterminal, right_hand_side)
   *)
  let make_parse_tracker gram = fun frag ->
    
    let start_symbol, production_function = gram in
    let alternative_list = production_function start_symbol in 

    let rec iterate_alternative nonterminal alternative_list accept (frag, traces) =
      match alternative_list with
      | [] -> None
      | alternative_list_head::alternative_list_tail -> 
        let acceptable_match = 
          (* keep track of rules applied *)
          iterate_right_hand_side 
            alternative_list_head 
            accept 
            (frag, traces @ [nonterminal, alternative_list_head]) 
        in
        match acceptable_match with
        | Some x -> Some x
        | None -> 
          iterate_alternative 
            nonterminal alternative_list_tail accept (frag, traces)

    and iterate_right_hand_side right_hand_side accept (frag, traces) =
      match right_hand_side, frag with
      | [], _ -> accept (frag, traces)
      | _, [] -> None
      | right_hand_side_head::right_hand_side_tail, frag_head::frag_tail ->
        match right_hand_side_head with
        | T terminal ->
          if terminal = frag_head
          then 
            iterate_right_hand_side 
              right_hand_side_tail accept (frag_tail, traces)
          else None
        | N nonterminal ->
          iterate_alternative 
            nonterminal
            (production_function nonterminal) 
            (iterate_right_hand_side right_hand_side_tail accept) 
            (frag, traces)
    in

    (*
     * acceptor for iterate_alternative and iterate_right_hand_side
     * frag must be parsed entirely, so we return None if suffix is not empty.
     * Instead of returning suffix, we want to return traces of rules applied.
     *)
    let accept_empty_suffix = function
      | _::_, _ -> None
      | _, traces -> Some traces
    in

    iterate_alternative 
      start_symbol alternative_list accept_empty_suffix (frag, [])
    
  in

  (* 
   * helper function for make_parser 
   * this function build a parse tree for given traces
   * and return parse_tree
   *)
  let build_parse_tree traces = 
  
    (*
     * helper function for build_parse_tree
     * build parse tree in dfs fashion
     * return (unused_traces, parse_tree) 
     *)
    let rec dfs traces =
      match traces with
      (* 
       * this case is not reachable
       * only for exhaustive pattern matching
       *)
      | [] -> failwith "traces can't be empty"
      | (nonterminal, right_hand_side)::traces_tail ->
        let unused_traces, parse_tree = 
          dfs_helper traces_tail right_hand_side 
        in
        unused_traces, Node (nonterminal, parse_tree)
    (*
     * helper function for dfs
     * build children parse trees from left to right
     * return (unused_traces, parse_tree list)
     *)
    and dfs_helper traces right_hand_side = 
      match right_hand_side with
      | [] -> traces, []
      | (T terminal)::right_hand_side_tail ->
        (* parse head of right_hand_side *)
        (* Leaf terminal *)
        (* parse tail of right_hand_side *)
        let unused_traces, parse_tree = 
          dfs_helper traces right_hand_side_tail 
        in
        (* combine the result *)
        unused_traces, (Leaf terminal)::parse_tree
      | (N nonterminal)::right_hand_side_tail ->
        (* parse head of right_hand_side *)
        let unused_traces, parse_tree_head = dfs traces in
        (* parse tail of right_hand_side *)
        let unused_unused_traces, parse_tree_tail = 
          dfs_helper unused_traces right_hand_side_tail 
        in
        (* combine the result *)
        unused_unused_traces, parse_tree_head::parse_tree_tail
    in

    (*
     * fst is [] 
     * snd has the actual parse tree we want
     *)
    snd (dfs traces)

  in

  (* None or traces wrapped by Some *)
  let wrapped_traces = (make_parse_tracker gram) frag in

  match wrapped_traces with
  | None -> None
  | Some traces -> Some (build_parse_tree traces)
    