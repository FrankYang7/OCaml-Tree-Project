
(* Below is a signature for binary search trees that exposes some
   implementation details to facilite testing.
 *)

module type  BSTreeImplS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  include BSTreeSig.BSTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_bst` may use a function 
     `min_tree` and thus add the following to this signature:

     val min_tree : 'a tree -> 'a option

     You may add what ever functions you like here. Then add
     tests for them in `BSTreeSet_Tests_Impl.ml`
   *)

end


module BSTreeImplM : BSTreeImplS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree


  let empty = Leaf
  (* This is the insert function to insert item in the Binary search tree.
     *)
  let rec insert (answer: 'a) (pre_tree: 'a t): 'a t =
    match pre_tree with
    | Leaf -> Fork  (Leaf , answer , Leaf)
    | Fork (l, v, r) -> if answer < v then Fork (insert answer l, v, r)
                      else if answer > v then Fork(l, v, insert answer r)
                      else Fork(l, v, r)

  let rec elem (answer: 'a) (given_tree: 'a t): bool =
    match given_tree with
    | Leaf -> false
    | Fork (l, v, r) -> if answer = v then true
                        else if answer < v then elem answer l
                        else elem answer r

  let rec height (given_tree: 'a t): int =
    match given_tree with
    | Leaf -> 0
    | Fork (l, v, r) -> if height l > height r then 1 + height l
                        else 1 + height r

  let rec size (given_tree: 'a t): int =
    match given_tree with
    | Leaf -> 0
    | Fork (l, v, r) -> (size l) + 1 + (size r)
  (* This is the helper function which is used to find the 
  minimum item in the given tree.
     *)
  let rec min_tree (given_tree: 'a t): 'a option =
      match given_tree with
      | Leaf -> None 
      | Fork (l, v, r) -> let small_l = min_tree l in
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
  (* This is the helper function which is used to
  find the maximum item in the given tree.
     *)
  let rec max_tree (given_tree: 'a t): 'a option =
    match given_tree with
    | Leaf -> None
    | Fork (l, v, r) -> let large_l = max_tree l in
                        let large_r = max_tree r in
                        if large_l > large_r && large_l > Some(v)
                          then large_l
                      else if large_r > large_l && large_r > Some(v)
                        then large_r
                      else Some(v)
  let rec is_bst (given_tree: 'a t): bool = 
    match given_tree with
    | Leaf -> true
    | Fork (l, v, r) -> let max_l = max_tree l in
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
                    
                           

end



(* Below we create a new moduled `BSTreeM` that only exposes the
   binary-search tree functionality in the BSTreeS` signature.
   Functions that may be useful for testing, such as `tree_min`
   are not accessible in `BSTreeM`.

   We "seal" `BSTreeM` with the signature `BSTreeS` so that it only
   exposes the elements of `BSTreeS`.
 *)
module BSTreeM : BSTreeSig.BSTreeS = BSTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `TreeM` are not
   accessible in `TreeSetM`.

   We "seal" `TreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module BSTreeSetM : SetSig.SetS = BSTreeM


