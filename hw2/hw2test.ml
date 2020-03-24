(*
 * this is the HW1-style grammar example
 *)
type awksub_nonterminals_1 =
  | Expr | Lvalue | Incrop | Binop | Num;;

let awksub_rules_1 =
  [
    Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]
  ]

let awksub_grammar_1 = Expr, awksub_rules_1;;

(*
 * test cases for convert_grammar
 * assume that you have convert_grammar implemented already
 *)
let awksub_grammar_1_hw2 = convert_grammar awksub_grammar_1;;

let test_start_symbol = 
  fst awksub_grammar_1 = fst awksub_grammar_1_hw2;;
let test_Expr = 
  (snd awksub_grammar_1_hw2) Expr = 
    [
      [T"("; N Expr; T")"];
      [N Num];
      [N Expr; N Binop; N Expr];
      [N Lvalue];
      [N Incrop; N Lvalue];
      [N Lvalue; N Incrop]
    ]
 
let test_Lvalue =
  (snd awksub_grammar_1_hw2) Lvalue = [[T"$"; N Expr]];;
 
let test_Incrop =
  (snd awksub_grammar_1_hw2) Incrop = [[T"++"];[T"--"]];;
 
let test_Binop =
  (snd awksub_grammar_1_hw2) Binop = [[T"+"];[T"-"]];;
 
let test_Num =
  (snd awksub_grammar_1_hw2) Num = 
    [[T"0"];[T"1"];[T"2"];[T"3"];[T"4"];[T"5"];[T"6"];[T"7"];[T"8"];[T"9"]];;

(* 
 * An example grammar for a small subset of Awk.
 * This grammar is not the same as Homework 1; it is
 * instead the same as the grammar under
 * "Theoretical background" above.  
 *)
type awksub_nonterminals_2 =
  | Expr | Term | Lvalue | Incrop | Binop | Num
  
let awkish_grammar_2 =
  (
    Expr,
    function
      | Expr ->
          [
            [N Term; N Binop; N Expr];
            [N Term]
          ]
      | Term ->
          [
            [N Num];
            [N Lvalue];
            [N Incrop; N Lvalue];
            [N Lvalue; N Incrop];
            [T"("; N Expr; T")"]
          ]
      | Lvalue ->
          [
            [T"$"; N Expr]
          ]
      | Incrop ->
          [
            [T"++"];
            [T"--"]
          ]
      | Binop ->
          [
            [T"+"];
            [T"-"]
          ]
      | Num ->
          [
            [T"0"]; 
            [T"1"]; 
            [T"2"]; 
            [T"3"]; 
            [T"4"];
            [T"5"]; 
            [T"6"];
            [T"7"]; 
            [T"8"]; 
            [T"9"]
          ]
  )

(* test cases for parse_tree_leaves *)
let parse_tree_leaves_test = parse_tree_leaves 
  (
    Node (
      Expr, [ 
        Node (
          Term, [
            Node (
              Num, [ Leaf "3" ]
            )
          ]
        ); 
        Node (
          Binop, [ Leaf "+" ]
        ); 
        Node (
          Expr, [ 
            Node (
              Term, [ 
                Node (
                  Num, [ Leaf "4" ]
                )
              ]
            )
          ]
        )
      ]
    )
  )
    = ["3"; "+"; "4"]

let parse_tree_leaves_test1 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
    = [3; 4; 5])
    

(*
 * acceptors 
 *)
let accept_all string = Some string
let accept_empty_suffix = function
  | _::_ -> None
  | x -> Some x

(* 
 * 5. Write one good, nontrivial test case for your make_matcher function. 
 * It should be in the style of the test cases given below, but should cover 
 * different problem areas. Your test case should be named make_matcher_test. 
 * Your test case should test a grammar of your own. 
 *)
type book_nonterminals = 
  | FullStmt | FullIf | Stmt | IfStmt | Expr 

let book_grammar =
  (
    Stmt,
    function 
      | Stmt -> 
        [
          [N IfStmt];
          [T "s1"];
          [T "s2"]
        ]
      | IfStmt ->
        [
          [T "if"; N Expr; T "then"; N FullStmt; T "else"; N Stmt];
          [T "if"; N Expr; T "then"; N Stmt]
        ]
      | FullStmt -> 
        [
          [N FullIf];
          [T "s1"];
          [T "s2"]
        ]
      | FullIf -> 
        [
          [T "if"; N Expr; T "then"; N FullStmt; T "else"; N FullStmt]
        ]
      | Expr -> 
        [
          [T "e1"];
          [T "e2"]
        ]
  )

(* test cases for make_matcher *)
let make_matcher_test = 
  (make_matcher book_grammar) 
    accept_all 
    ["if"; "e1"; "then"; "if"; "e2"; "then"; "s1"; "else"; "s2"] 
      = Some []

let make_matcher_test1 = 
  (make_matcher book_grammar) 
    accept_all 
    ["if"; "e1"; "then"; "if"; "e2"; "then"; "s1"; "else"; "s2"; "if"; "e1"; "then"; "e2"] 
      = Some ["if"; "e1"; "then"; "e2"]

let make_matcher_test2 = 
  (make_matcher book_grammar) 
    accept_empty_suffix 
    ["if"; "e1"; "then"; "if"; "e2"; "then"; "s1"; "else"; "s2"; "if"; "e1"; "then"; "e2"]
      = None

let make_matcher_test3 = 
  (make_matcher awkish_grammar_2) accept_all ["ouch"] = None

let make_matcher_test4 =
  (make_matcher awkish_grammar_2) accept_all ["9"] = Some []

let make_matcher_test5 =
  (make_matcher awkish_grammar_2) accept_all ["9"; "+"; "$"; "1"; "+"]
    = Some ["+"]

let make_matcher_test6 =
  (make_matcher awkish_grammar_2) accept_empty_suffix ["9"; "+"; "$"; "1"; "+"]
    = None

let make_matcher_test7 =
  (make_matcher awkish_grammar_2) accept_all
    [
      "("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";"("; "$"; "++";
      "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";"-"; "("; "$"; "$"; "$"; "$";
      "$"; "++"; "$"; "$"; "5"; "++";"++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; 
      "$"; "8"; "++"; ")";"++"; "+"; "0"
    ]
      = Some []

let make_matcher_test8 =
  (make_matcher awkish_grammar_2) accept_all ["$"; "("; "("; "$"; "8"; "++"; ")"; ")"]
    = Some []

(* 
 * 6. Similarly, write a good test case make_parser_test for your make_parser 
 * function using your same test grammar. This test should check that 
 * parse_tree_leaves is in some sense the inverse of make_parser gram, 
 * in that when make_parser gram frag returns Some tree, then parse_tree_leaves 
 * tree equals frag. 
 *)
(* test cases for make_parser *)
let make_parser_test0 = 
  match make_parser awkish_grammar_2 ["9"] with
  | None -> false
  | Some tree -> parse_tree_leaves tree = ["9"]

let make_parser_test1 = 
match (make_parser awkish_grammar_2) ["9"; "+"; "$"; "1"] with
| None -> false
| Some tree -> parse_tree_leaves tree = ["9"; "+"; "$"; "1"]

let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let make_parser_test2 =
  ((make_parser awkish_grammar_2 small_awk_frag)
    = Some (Node (Expr,
      [Node (Term,
      [Node (Lvalue,
              [Leaf "$";
        Node (Expr,
              [Node (Term,
                [Node (Num,
                [Leaf "1"])])])]);
        Node (Incrop, [Leaf "++"])]);
      Node (Binop,
      [Leaf "-"]);
      Node (Expr,
      [Node (Term,
              [Node (Num,
              [Leaf "2"])])])])))

let make_parser_test3 =
  match make_parser awkish_grammar_2 small_awk_frag with
    | Some tree -> parse_tree_leaves tree = small_awk_frag
    | _ -> false
