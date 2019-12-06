(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec removeExnIter : xexp -> xexp = fun e ->
  let k = new_name () in
  let h = new_name () in
  match e with
  | Xexp.Num n -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, Xexp.Num n)))
  | Xexp.Var id -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, Xexp.Var id)))
  | Xexp.Fn (f, e) -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, Xexp.Fn (f, removeExnIter e))))
  | Xexp.App (fn, arg) -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, e)))
  | Xexp.If (cond, t, f) ->
    let c = new_name () in
    let s = new_name () in
    Xexp.Fn (k, Xexp.Fn (h,
      Xexp.App (Xexp.App (removeExnIter cond,
        Xexp.Fn (c,
          Xexp.If (Xexp.Var c,
            Xexp.App (Xexp.App (removeExnIter t,
              Xexp.Fn (s,
                Xexp.App (Xexp.Var k, Xexp.Var s)
              )
            ), Xexp.Var h),
            Xexp.App (Xexp.App (removeExnIter f,
              Xexp.Fn (s,
                Xexp.App (Xexp.Var k, Xexp.Var s)
              )
            ), Xexp.Var h)
          )
        )
      ), Xexp.Var h)
    ))
  | Xexp.Equal (left, right) ->
    let l = new_name () in
    let r = new_name () in
    Xexp.Fn (k, Xexp.Fn (h,
      Xexp.App (Xexp.App (removeExnIter left,
        Xexp.Fn (l,
          Xexp.App (Xexp.App (removeExnIter right,
            Xexp.Fn (r,
              Xexp.App (Xexp.Var k, Xexp.Equal (Xexp.Var l, Xexp.Var r))
            )
          ), Xexp.Var h)
        )
      ), Xexp.Var h)
    ))
  | Xexp.Raise expn -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, e)))
  | Xexp.Handle (t, error_code, error) -> Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, e)))

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  let x = new_name () in
  let y = new_name () in
  Xexp.App (Xexp.App (removeExnIter e, Xexp.Fn (x, Var x)), Xexp.Fn (y, Num 201912))
