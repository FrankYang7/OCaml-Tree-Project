open SetSig
open Hidden_Words_Sig
open Util

(* This module is meant to include only tail recursive functions.
 *)
module HiddenImpl_TR_F (S: SetS)  = struct
  (* select_word_list function is used to select the word with target length. 
     *)
  let select_word_list (given_list:string list) (num: int): 'a list =
    let rec select_tail (list_so_far: 'a list) (lst: 'a list): 'a list =
      match lst with
      | [] -> list_so_far
      | x::xs -> if (String.length x = num) then select_tail ([x] @ list_so_far ) xs
                  else select_tail list_so_far xs
    in
    select_tail [] given_list
  (* The combine_single function is used to combine the three letter length word
     and the five letter length word and then output the 4 eight letter length words.
     *)

  let combine_single (three_letter_word: string) (five_letter_word: ('a * 'a) list): string list =
    let rec combine_tail (list_so_far: string list) (char_remain: ('a * 'a) list): string list =
    match char_remain with
    | [] -> list_so_far
    | (x1,x2)::xs -> if x1 != [] && x2 != [] then (UtilM.implode (x1 @ (UtilM.explode three_letter_word) @ x2):: combine_tail list_so_far xs)
    else combine_tail list_so_far xs
  in
  combine_tail [] five_letter_word
  (* The combine_list is used to generate the (string * string * string) type by using the combined words.
     *)
  let combine_list (three_letter_word: string) (five_letter_word: string list): (string*string*string) list =
    let rec combine_list_tail (list_so_far: (string * string * string) list) (remain_list: string list): (string * string * string) list =
    match remain_list with
    | [] -> list_so_far
    | x::xs -> let five_char = UtilM.all_parts (UtilM.explode x) in
               let comb = combine_single three_letter_word five_char in
               let rec generate_sp (three:string) (five:string) (g:string list): (string * string * string) list =
                match g with
                | [] -> []
                | y::ys -> (three, five, y)::generate_sp three five ys
               in
                 combine_list_tail ((generate_sp three_letter_word x comb) @ list_so_far) xs
    in
    combine_list_tail [] five_letter_word
  (* The combine_answer is generate all the three letter length word and five letter length word in a single list.
       *)
  let combine_answer (three_list: string list) (five_list: string list): (string*string*string) list =
    let rec answer_tail (list_so_far: (string * string * string) list) (remain_list: string list): (string*string*string)list =
    match remain_list with
    | [] -> list_so_far
    | x::xs ->  answer_tail ((combine_list x five_list) @ list_so_far) xs
    in
    answer_tail [] three_list
  (* final_check function is used to call elem function to check each answer in the (three * five * answer).
     *)
  let final_check  (answer_list: (string* string* string) list ) (answer_t: string S.t): (string * string * string) list =
    let rec final_tail (list_so_far: (string * string * string) list) (remain_list: (string * string * string) list): (string * string * string) list =
    match remain_list with
    | [] -> list_so_far
    | (x1,x2,x3)::xs -> if (S.elem x3 answer_t = true) then final_tail ([(x1,x2,x3)] @ list_so_far) xs
    else final_tail list_so_far xs 
    in
    final_tail [] answer_list
  (* hidden_words function is used to generate the final answer.
     *)
  let hidden_words (word_list: string list) : (string * string * string) list =
    let three_list = select_word_list word_list 3 in
    let five_list = select_word_list word_list 5 in
    let answer_list = select_word_list word_list 8 in
    let p_answer = combine_answer three_list five_list in
    final_check p_answer (List.fold_right S.insert answer_list S.empty)


end


(* Here we dfined the functor `HiddenF`. It uses the `HiddenImplF`
   functor but only exposes the `hidden_words` function that is
   defined in `HiddenS`.

   We will use `HiddenF` in `solution.ml` to create the list-based
   and the tree-based solutions to the problem.
 *)

module HiddenWords_TR_F (S: SetS) : HiddenS = HiddenImpl_TR_F(S)
