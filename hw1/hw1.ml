(* 1. Write a function subset a b that returns true iff a⊆b, 
 * i.e., if the set represented by the list a is a subset of the 
 * set represented by the list b. Every set is a subset of itself. 
 * This function should be generic to lists of any type: that is, 
 * the type of subset should be a generalization of 
 * 'a list -> 'a list -> bool. 
 *)
let rec subset a b =
	match a with
	| [] -> true
	| head::tail -> 
		if List.mem head b then subset tail b 
		else false


(* 2. Write a function equal_sets a b that returns true iff 
 * the represented sets are equal.
 *)
let equal_sets a b = subset a b && subset b a


(* 3. Write a function set_union a b that returns a list 
 * representing a∪b.
 *)
let rec set_union a b = List.append a b


(* 4. Write a function set_intersection a b that returns a list 
 * representing a∩b.
 *)
 let set_intersection a b = 
 	List.filter (fun element -> List.mem element b) a

 (* 5. Write a function set_diff a b that returns a list 
  * representing a−b, that is, the set of all members of a 
  * that are not also members of b.
  *)
let set_diff a b = 
	List.filter (fun element -> not (List.mem element b)) a


(* 6. Write a function computed_fixed_point eq f x that returns the 
 * computed fixed point for f with respect to x, assuming that eq is 
 * the equality predicate for f's domain. A common case is that eq 
 * will be (=), that is, the builtin equality predicate of OCaml; 
 * but any predicate can be used. If there is no computed fixed 
 * point, your implementation can do whatever it wants: for example, 
 * it can print a diagnostic, or go into a loop, or send nasty email 
 * messages to the user's relatives.
 *)
 let rec computed_fixed_point eq f x =
 	if eq (f x) x then x
 	else computed_fixed_point eq f (f x)


(* 7. OK, now for the real work. Write a function filter_reachable g 
 * that returns a copy of the grammar g with all unreachable rules 
 * removed. This function should preserve the order of rules: 
 * that is, all rules that are returned should be in the same order 
 * as the rules in g.
 *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let filter_reachable g = 
	let start_symbol, rules = g in

	(*******************
	 * helper functions 
	 *******************)
	(* apply equal_sets to second element of pairs *)
	let equal_second_element_sets a b =
		let _, a_second = a and _, b_second = b in
		equal_sets a_second b_second
	in
	(* filter right hand side 
	 * and only allow non-terminal to be returned *)
	let rec filter_terminal symbols = 
		match symbols with
		| [] -> []
		| N head::tail -> head::(filter_terminal tail)
		| T head::tail -> filter_terminal tail
	in
	(* get reachable symbols from currently reachable symbols *)
	let rec get_reachable_symbols params = 

		let current_rules, reachable_symbols = params in
		
		match current_rules with
		| [] -> rules, reachable_symbols
		| rules_head::rules_tail -> 
			(* check first element of the rule list *)
			let symbol, right_hand_side = rules_head in
			(* if the left hand side is reachable, 
			 * we add non termianls in the right hand side
			 * to the list of reachable symbols.
			 *)
			if List.mem symbol reachable_symbols then
				let nonterminal = filter_terminal right_hand_side in
				get_reachable_symbols 
					(rules_tail, set_union reachable_symbols nonterminal)
			else get_reachable_symbols (rules_tail, reachable_symbols)
	in

	(* step 1: find the reachable symbols *)
	let _, reachable_symbols = 
		computed_fixed_point 
			equal_second_element_sets 
			get_reachable_symbols 
			(rules, [start_symbol]) 
	in

	(* step 2: filter the rules *)
	let filtered_rules = List.filter 
		(fun (symbol, _) -> List.mem symbol reachable_symbols) 
		rules 
	in
	start_symbol, filtered_rules