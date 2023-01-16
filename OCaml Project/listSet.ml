open SetSig

(* Below add you implementation of sets as lists. Since these are
   pretty simple we do not build a separate version for testing 
   like we see in `BSTreeSet.ml` or `RBTreeSet.ml`.
 *)

module ListSetM : SetS = struct

  (* implode function could convert char list to string.
  *)
  type 'a t = 'a list
  let empty = []
  let rec insert (answer: 'a) (pre_list: 'a t): 'a t =
    match pre_list with
    | [] -> answer :: []
    | x::xs -> answer::x::xs
  let rec elem (answer: 'a) (given_list: 'a t): bool =
    match given_list with
    | [] -> false
    | x::xs -> if x = answer then true 
              else elem answer xs
end
  