open SetSig
open Util
open Hidden_Words_Sig

let new_1 = [ 
  "act"; "doldrums"; "drums"; "era"; "forth"; "fortieth"; "moribund"; 
  "mound"; "old"; "operatic"; "optic"; "practice"; "price"; "protrude"; 
  "prude"; "ran"; "relet"; "relevant"; "rib"; "rot"; "spa"; "tie"; 
  "trespass"; "tress"; "van"; "warranty"; "warty"
]
let rec select_word_list (given_list:string list) (num: int): 'a list =
  match given_list with
  | [] -> []
  | x::xs -> if (String.length x = num) then x::(select_word_list xs num)
    else select_word_list xs num
let rec combine_single (three_letter_word: string) (five_letter_word: ('a * 'a) list): string list =
  match five_letter_word with
  | [] -> []
  | (x1,x2)::xs -> if x1 != [] && x2 != [] then (UtilM.implode (x1 @ (UtilM.explode three_letter_word) @ x2):: combine_single three_letter_word xs)
                  else combine_single three_letter_word xs
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
let rec combine_answer (three_list: string list) (five_list: string list): (string*string*string) list =
  match three_list with
  | [] -> []
  | x::xs -> (combine_list x five_list) @ (combine_answer xs five_list)

