
(* Below is a signature for red-black trees that exposes some
   implementation details to facilitate testing.
 *)

module type  RBTreeImplS = sig

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  include RBTreeSig.RBTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_red_black_tree` may use helper
     function and you may add them here to facilitate testing.
   *)

end

module RBTreeImplM : RBTreeImplS = struct

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty = E

  (* insert function is used to insert the new element in the given 'a t type.
     The balance technical is referenced from S7.2. 
     *)
  let rec insert (answer: 'a) (given_tree: 'a t): 'a t =
    let balance c t1 v t2 =
      match c, t1, v, t2 with
      | B, T(R, T(R,a, x, b), y, c), z, d
      | B, T(R, a, x, T(R, b, y, c)), z, d
      | B, a, x, T(R, T(R, b, y, c), z, d)
      | B, a, x, T(R, b, y, T(R, c, z, d))
      -> T(R, T(B, a, x, b), y, T(B, c, z, d))
      | c , t1, v, t2 -> T(c, t1, v, t2)
    in
    let rec ins (given_tree': 'a t): 'a t =
      match given_tree' with
      | E -> T(R, E, answer, E)
      | T(c, t1, y, t2) -> 
        if answer < y
        then balance c (ins t1) y t2
        else if answer > y
        then balance c t1 y (ins t2)
        else given_tree'
      in
      match ins given_tree with
      | E -> failwith "Not exist case"
      | T (_, t1, y, t2) -> T(B, t1, y, t2)
  let rec elem (answer: 'a) (given_tree: 'a t): bool =
    match given_tree with
    | E -> false
    | T(_, l, v, r) -> if answer = v then true
                      else if answer < v then elem answer l
                      else elem answer r

  let rec height (given_tree: 'a t): int =
    match given_tree with
    | E -> 0
    | T (c, l, v, r) -> if height l > height r then 1 + height l
                        else 1 + height r

  let rec size (given_tree: 'a t): int =
    match given_tree with
    | E -> 0
    | T (c, l, v, r) -> (size l) + 1 + (size r)
  let rec min_tree (given_tree: 'a t): 'a option =
    match given_tree with
      | E -> None 
      | T (_, l, v, r) -> let small_l = min_tree l in
                          let small_r = min_tree r in
                          if small_l = None && small_r = None
                            then Some(v)
                        else if small_l = None
                          then 
                            if small_r < Some(v)
                            then small_r
                      else Some(v)
                    else if small_r = None
                      then 
                        if small_l < Some(v)
                        then small_l
                  else Some(v)
                else
                  if small_l < Some(v) && small_l < small_r
                    then small_l
              else if small_r < Some(v) && small_r < small_l
                then small_r
              else Some(v)
  let rec max_tree (given_tree: 'a t): 'a option =
    match given_tree with
    | E -> None
    | T (_, l, v, r) -> let large_l = max_tree l in
                        let large_r = max_tree r in
                        if large_l > large_r && large_l > Some(v)
                          then large_l
                      else if large_r > large_l && large_r > Some(v)
                        then large_r
                      else Some(v)
  let rec is_bst (given_tree: 'a t): bool =
    match given_tree with
    | E -> true
    | T (_, l, v, r) -> let max_l = max_tree l in
                        let min_r = min_tree r in
                        if is_bst l = false || is_bst r = false
                          then false
                      else 
                        if max_l = None && min_r = None
                          then true
                    else if max_l = None
                      then 
                        if min_r > Some(v)
                          then true
                    else false
                    else if min_r = None
                      then 
                        if max_l < Some(v)
                          then true
                  else false
                else
                  if max_l < Some(v) && min_r > Some(v) 
                    then true
              else false
  (* count_longest_b function is used to find the longest path's 
  black node's number. This is a helper function.
     *)
  let rec count_longest_b (given_tree: 'a t): int =
    match given_tree with
    | E -> 0
    | T (c, l, v, r) -> if c = B
                        then if height l > height r
                          then 1+(count_longest_b l)
                          else 1+(count_longest_b r)
                        else 
                          if height l > height r
                            then count_longest_b l
                      else count_longest_b r
  (* This count_Bnode function is test whether the black node on each path
  have same number. This is a helper function.
     *)
  let rec count_Bnode (given_tree: 'a t) (check_num: int) (current_num: int): bool=
    match given_tree with
    | E -> true
    | T (c, l, v, r) ->  (* if c = B is the case of the current color is Black*)
                         if c = B 
                          then 
                            if l != E && r != E
                              then 
                                if ((count_Bnode l check_num (current_num+1) ) = true) && ((count_Bnode r check_num (current_num+1)) = true)
                                  then true
                                else false
                            else if l = E && r != E
                              then 
                                if (check_num = current_num+1) && ((count_Bnode r check_num (current_num+1)) = true)
                                  then true
                          else false
                        else if r = E && l != E
                          then 
                            if (check_num = current_num+1) && ((count_Bnode l check_num (current_num+1)) = true)
                              then true
                      else false
                    else 
                      if (check_num = current_num +1)
                        then true
                  else false
                  (*This is the current color = Red case. *)
                    else
                      if l != E && r != E
                        then 
                          if ((count_Bnode l check_num current_num) = true) && ((count_Bnode r check_num current_num) = true)
                            then true
                          else false
                      else if l = E && r != E
                        then 
                          if (check_num = current_num) && ((count_Bnode r check_num current_num) = true)
                            then true
                    else false
                  else if r = E && l != E
                    then 
                      if (check_num = current_num) && ((count_Bnode l check_num current_num) = true)
                        then true
                else false
              else 
                if (check_num = current_num)
                  then true
            else false
  (* The check_color function is used to check the current Node's color.
     *)
  let check_color (given_tree: 'a t): color =
    match given_tree with
    | E -> B
    | T(c, l, v, r) -> c
  (* is_red_black_tree is used to check RBTree.
     *)
  let is_red_black_tree (given_tree: 'a t): bool =
    let rec check_red (new_tree: 'a t): bool =
    match new_tree with
    | E -> true
    | T(c, l, v, r) -> if c = B 
                        then if (check_red l = true)&& (check_red r = true)
                          then true
                        else
                          false
                      else
                        if (check_color l = B)&&(check_color r = B) && (check_red l = true) && (check_red r = true)
                          then true
                        else false
    in
    if (count_Bnode given_tree (count_longest_b given_tree) 0) = true && check_red given_tree = true
      then true
  else false
    

                        

                          

end



(* Below we create a new modules `RBTreeM` that only exposes the
   red-black tree functionality in the `RBTreeS` signature.
   Functions that may be useful for testing, such as `all_paths`
   are not accessible in `RBTreeM`.

   We "seal" `RBTreeM` with the signature `RBTreeS` so that it only
   exposes the elements of `RBTreeS`.   
*)
module RBTreeM : RBTreeSig.RBTreeS = RBTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeBSTM : BSTreeSig.BSTreeS = RBTreeImplM



(* Below we create a new module `RBTreeSetM` that only exposes the
   set functionality in the `SetSig` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeSetM : SetSig.SetS = RBTreeImplM
