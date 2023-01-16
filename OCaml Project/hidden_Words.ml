open SetSig
open Hidden_Words_Sig
open Util


(* Here, we choose to not specify a "implementation" signature like
   `BSTreeImplS` one defined in `BSTree.ml`. Instead we leave the
   result of the functor to be "open" - that is, not sealed by any
   signature.
*)
module HiddenWordsImplF (S: SetS)  = struct
  (* select_word_list function is used to select the word with target length. 
     *)
  let rec select_word_list (given_list:string list) (num: int): 'a list =
    match given_list with
    | [] -> []
    | x::xs -> if (String.length x = num) then x::(select_word_list xs num)
      else select_word_list xs num
  (* The combine_single function is used to combine the three letter length word
     and the five letter length word and then output the 4 eight letter length words.
     *)
  let rec combine_single (three_letter_word: string) (five_letter_word: ('a * 'a) list): string list =
    match five_letter_word with
    | [] -> []
    | (x1,x2)::xs -> if x1 != [] && x2 != [] then (UtilM.implode (x1 @ (UtilM.explode three_letter_word) @ x2):: combine_single three_letter_word xs)
    else combine_single three_letter_word xs
  (* The combine_list is used to generate the (string * string * string) type by using the combined words.
     *)
  let rec combine_list (three_letter_word: string) (five_letter_word: string list): (string*string*string) list =
    match five_letter_word with
    | [] -> []
    | x::xs -> let five_char = UtilM.all_parts (UtilM.explode x) in
               let comb = combine_single three_letter_word five_char in
               let rec generate_sp (three:string) (five:string) (g:string list): (string * string * string) list =
                match g with
                | [] -> []
                | y::ys -> (three, five, y)::generate_sp three five ys
               in
               (generate_sp three_letter_word x comb) @ combine_list three_letter_word xs 
  (* The combine_answer is generate all the three letter length word and five letter length word in a single list.
       *)
  let rec combine_answer (three_list: string list) (five_list: string list): (string*string*string) list =
    match three_list with
    | [] -> []
    | x::xs -> (combine_list x five_list) @ (combine_answer xs five_list)
  (* final_check function is used to call elem function to check each answer in the (three * five * answer).
     *)
  let rec final_check  (answer_list: (string* string* string) list ) (answer_t: string S.t): (string * string * string) list =
    match answer_list with
    | [] -> []
    | (x1,x2,x3)::xs -> if (S.elem x3 answer_t = true) then (x1,x2,x3)::final_check xs answer_t
    else final_check xs answer_t
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

module HiddenWordsF (S: SetS) : HiddenS = HiddenWordsImplF(S)
