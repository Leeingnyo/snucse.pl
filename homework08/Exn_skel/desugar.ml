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
  | Xexp.App (fn, arg) ->
    (* \K,H. T(e1) <\f. T(e2) <\v. f v <K,H>, H>, H> *)
    let f = new_name () in
    let v = new_name () in
    Xexp.Fn (k, Xexp.Fn (h, (* \K,H. *)
      Xexp.App (Xexp.App (removeExnIter fn, (* T(e1) < *)
        Xexp.Fn (f, (* \f. *)
          Xexp.App (Xexp.App (removeExnIter arg, (* T(e2) < *)
            Xexp.Fn (v, (* \v. *)
              Xexp.App (Xexp.App (Xexp.App (Xexp.Var f, Xexp.Var v), Xexp.Var k), Xexp.Var h) (* f v <K,H> *)
            )
          ), Xexp.Var h) (* , H> *)
        )
      ), Xexp.Var h) (* , H> *)
    ))
    (*
    Xexp.Fn (k, Xexp.Fn (h, Xexp.App (Xexp.Var k, e)))
    *)
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
  | Xexp.Raise expn ->
    Xexp.Fn (k, Xexp.Fn (h,
      Xexp.App (Xexp.App (removeExnIter expn, Xexp.Var h), Xexp.Var h)
    ))
  | Xexp.Handle (t, error_code, error) ->
    let v = new_name () in
    Xexp.Fn (k, Xexp.Fn (h,
      Xexp.App (Xexp.App (removeExnIter t, Xexp.Var k),
        Xexp.Fn (v,
          Xexp.If (Xexp.Equal (Xexp.Var v, Xexp.Num error_code),
            (* 에러 코드가 같으면 여기서 처리 *)
            App (App (removeExnIter error, Var k), Var h),
            (* 에러 코드가 다르면 다음에 처리 *)
            Xexp.App (Xexp.Var h, Xexp.Var v)
          )
        )
      )
    ))

(* TODO : Implement this function *)
let removeExn : xexp -> xexp = fun e ->
  let x = new_name () in
  let y = new_name () in
  Xexp.App (Xexp.App (removeExnIter e, Xexp.Fn (x, Var x)), Xexp.Fn (y, Num 201912))
