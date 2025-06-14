(* Do not open any module *)

(***********************)
(*  Library functions  *)
(***********************)

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

(***********************)
(******  Syntax  *******)
(***********************)

type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

(***********************)
(**  Example programs **)
(***********************)

(*
  let f = proc (x) (x - 11)
  in (f (f 77))
*)
let proc1 = 
  LET ("f", PROC ("x", SUB (VAR "x", CONST 11)),
    CALL (VAR "f", CALL (VAR "f", CONST 77)))

(*
  ((proc (f) (f (f 77))) (proc (x) (x-11)))
*)
let proc2 = 
  CALL (PROC ("f", CALL (VAR "f", CALL (VAR "f", CONST 77))), 
        PROC ("x", SUB (VAR "x", CONST 11)))

(*
  let x = 1
  in let f = proc (y) (x + y)
     in let x = 2
        in let g = proc (y) (x + y)
        in  (f 1) + (g 1)
*)
let let1 = 
  LET ("x", CONST 1, 
    LET ("f", PROC ("y", ADD (VAR "x", VAR "y")),
      LET ("x", CONST 2, 
         LET ("g", PROC ("y", ADD (VAR "x", VAR "y")), 
            (ADD (CALL (VAR "f", CONST 1), 
                  CALL (VAR "g", CONST 1)))))))

(*
  letrec even(x) = if (x = 0) then true else odd(x-1)
         odd(x) = if (x = 0) then false else even(x-1)
  in (even 13)
*)
let evenodd = 
  LETMREC (("even", "x", IF (EQUAL (VAR "x", CONST 0), TRUE, CALL (VAR "odd",  SUB (VAR "x", CONST 1)))),
           ("odd" , "x", IF (EQUAL (VAR "x", CONST 0), FALSE, CALL (VAR "even", SUB (VAR "x", CONST 1)))),
  CALL (VAR "odd", CONST 13))


(*
  letrec double(x) = if (x = 0) then 0 else (double (x-1) + 2
  in (double 6)
*)
let double = 
  LETREC ("double", "x", IF (EQUAL (VAR "x", CONST 0), 
                            CONST 0, 
                            ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)) , 
                                 CONST 2)), 
    CALL (VAR "double", CONST 6))

(*
letrec factorial(x) = 
         if (x = 0) then 1 
         else factorial(x-1) * x
in letrec loop n = 
     if (n = 0) then ()
     else (print (factorial n); loop (n-1))
   in (loop 10)
*)
let fact = 
LETREC ("factorial", "x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "factorial", SUB (VAR "x", CONST 1)), VAR "x")), 
  LETREC ("loop", "n", 
    IF (EQUAL (VAR "n", CONST 0), UNIT, 
        SEQ (PRINT (CALL (VAR "factorial", VAR "n")), 
             CALL (VAR "loop", SUB(VAR "n", CONST 1)))), 
      CALL (VAR "loop", CONST 10)))
           
(*
in letrec range(n) = 
      if (n = 1) then (cons 1 nil)
      else n::(range (n-1))
in (range 10)
*)
let range = 
LETREC ("range", "n", 
            IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1)))), 
     CALL (VAR "range", CONST 10))

(*
letrec reverse(l) = 
  if (isnil l) then []
  else (reverse (tl l)) @ (cons hd l)
in (reverse (cons (1, cons (2, cons (3, nil)))))
*)
let reverse = 
LETREC ("reverse", "l", 
          IF (ISNIL (VAR "l"), NIL, 
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), 
                      CONS (HEAD (VAR "l"), NIL))), 
     CALL (VAR "reverse", 
           CONS (CONST 1, CONS (CONST 2, CONS (CONST 3, NIL)))))

let reverse2 = 
LETREC ("reverse", "l", 
          IF (ISNIL (VAR "l"), NIL, 
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), 
                      CONS (HEAD (VAR "l"), NIL))), 
     CALL (VAR "reverse", 
           CONS (CONS (CONST 1, NIL), CONS (CONS (CONST 2, NIL), CONS (CONS (CONST 3, NIL), NIL)))))


let zfact = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
    LET ("f", CALL (VAR "fix", 
            PROC ("f", PROC ("x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x"))))), 
           CALL (VAR "f", CONST 10)))

let zrange = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),


    LET ("f", CALL (VAR "fix", 
            PROC ("range", PROC ("n", 
               IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                 CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1))))))), 
           CALL (VAR "f", CONST 10)))

let poly = 
    LET ("f", PROC("x", VAR "x"), 
      IF(CALL (VAR "f", TRUE), CALL (VAR "f", CONST 1), CALL (VAR "f", CONST 2)))

let lst =
    CONS (CONST 1, CONS (CONST 2, CONS (TRUE, NIL)))

(***********************)
(*****  Problem 1  *****)
(***********************)

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp *
                     var * var * exp * env
and env = (var * value) list

exception UndefinedSemantics

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR x -> lookup_env x env
  | ADD (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (Int v1, Int v2) -> Int (v1 + v2)
    | _ -> raise UndefinedSemantics)
  | SUB (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (Int v1, Int v2) -> Int (v1 - v2)
    | _ -> raise UndefinedSemantics)
  | MUL (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (Int v1, Int v2) -> Int (v1 * v2)
    | _ -> raise UndefinedSemantics)
  | DIV (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (_, Int 0) -> raise UndefinedSemantics
    | (Int v1, Int v2) -> Int (v1 / v2)
    | _ -> raise UndefinedSemantics)
  | EQUAL (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (Int v1, Int v2) -> Bool (v1 = v2)
    | (Bool b1, Bool b2) -> Bool (b1 = b2)
    | _ -> raise UndefinedSemantics)
  | LESS (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (Int v1, Int v2) -> Bool (v1 < v2)
    | _ -> raise UndefinedSemantics)
  | NOT e ->
    (match eval e env with
    | Bool b -> Bool (not b)
    | _ -> raise UndefinedSemantics)
  | NIL -> List []
  | CONS (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (v, List lst) -> List (v :: lst)
    | _ -> raise UndefinedSemantics)
  | APPEND (e1, e2) ->
    (match (eval e1 env, eval e2 env) with
    | (List lst1, List lst2) -> List (lst1 @ lst2)
    | _ -> raise UndefinedSemantics)
  | HEAD e ->
    (match eval e env with
    | List (hd :: _) -> hd
    | List [] -> raise (Failure "head of empty list")
    | _ -> raise UndefinedSemantics)
  | TAIL e ->
    (match eval e env with
    | List (_ :: tl) -> List tl
    | List [] -> raise (Failure "tail of empty list")
    | _ -> raise UndefinedSemantics)
  | ISNIL e ->
    (match eval e env with
    | List [] -> Bool true
    | List _ -> Bool false
    | _ -> raise UndefinedSemantics)
  | IF (e1, e2, e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise UndefinedSemantics)
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in
    let new_env = extend_env (x, v1) env in eval e2 new_env
  | LETREC (f, x, e1, e2) ->
    let env' = extend_env (f, RecProcedure (f, x, e1, env)) env in eval e2 env'
  | LETMREC ((f1, x1, e1), (f2, x2, e2), e3) ->
    let env' =
      extend_env (f1, MRecProcedure (f1, x1, e1, f2, x2, e2, env))
      (extend_env (f2, MRecProcedure (f2, x2, e2, f1, x1, e1, env)) env) in eval e3 env'
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) ->
    (match eval e1 env with
    | Procedure (x, body, env') ->
      let arg = eval e2 env in
      let new_env = extend_env (x, arg) env' in
      eval body new_env
    | RecProcedure (f, x, body, env') ->
      let arg = eval e2 env in
      let new_env = extend_env (x, arg) @@ extend_env (f, RecProcedure (f, x, body, env')) env in eval body new_env
    | MRecProcedure (f1, x1, body1, f2, x2, body2, env') ->
      let arg = eval e2 env in
      let new_env = extend_env (x1, arg) @@
                    extend_env (f1, MRecProcedure (f1, x1, body1, f2, x2, body2, env')) @@
                    extend_env (x2, arg) @@
                    extend_env (f2, MRecProcedure (f1, x1, body1, f2, x2, body2, env')) @@
                    env in eval body1 new_env
    | _ -> raise UndefinedSemantics)
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | SEQ (e1, e2) -> let _ = eval e1 env in eval e2 env
  

let runml : program -> value
=fun pgm -> eval pgm empty_env


(***********************)
(*****  Problem 2  *****)
(***********************)

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string

exception TypeError

(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

type t_t = var -> typ
let t_empty = fun _ -> raise (Failure "Type Env is empty")
let t_extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
let t_find tenv x = tenv x

type s_t = (tyvar * typ) list
let s_empty = []
let rec s_find x subst =   
  match subst with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in subst")) 
  | (y,v)::tl -> if x = y then v else s_find x tl

let rec s_apply : typ -> s_t -> typ
=fun typ subst ->
  match typ with
  | TyUnit -> TyUnit
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (t1,t2) -> TyFun (s_apply t1 subst, s_apply t2 subst)
  | TyList t -> TyList (s_apply t subst)
  | TyVar x ->
    try s_find x subst
    with _ -> typ
  
let s_extend tv ty subst =
  (tv,ty) ::
  (map (fun (x,t) ->
  (x, s_apply t [(tv,ty)])) subst)


type typ_eqn = (typ * typ) list

let rec gen_equations : t_t -> exp -> typ -> typ_eqn
=fun tenv e ty ->
  match e with
  | UNIT -> [(ty, TyUnit)]
  | TRUE | FALSE -> [(ty, TyBool)]
  | CONST _ -> [(ty, TyInt)]
  | VAR x -> [(ty, tenv x)]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) ->
      (ty, TyInt) :: gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
  | EQUAL (e1, e2) ->
      (ty, TyBool) :: gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
  | LESS (e1, e2) ->
      (ty, TyBool) :: gen_equations tenv e1 TyInt @ gen_equations tenv e2 TyInt
  | NOT e ->
      (ty, TyBool) :: gen_equations tenv e TyBool
  | NIL -> [(ty, TyList (fresh_tyvar ()))]
  | CONS (e1, e2) ->
      let t1 = fresh_tyvar () in
      let t2 = TyList (t1) in
      (ty, t2) :: gen_equations tenv e1 t1 @ gen_equations tenv e2 t2
  | APPEND (e1, e2) ->
      let t = fresh_tyvar () in
      (ty, TyList (t)) :: gen_equations tenv e1 (TyList (t)) @ gen_equations tenv e2 (TyList (t))
  | HEAD e ->
      let t = fresh_tyvar () in
      (ty, t) :: gen_equations tenv e (TyList (t))
  | TAIL e ->
      let t = fresh_tyvar () in
      (ty, TyList (t)) :: gen_equations tenv e (TyList (t))
  | ISNIL e ->
      (ty, TyBool) :: gen_equations tenv e (TyList (fresh_tyvar ()))
  | IF (e1, e2, e3) ->
      let eqns1 = gen_equations tenv e1 TyBool in
      let eqns2 = gen_equations tenv e2 ty in
      let eqns3 = gen_equations tenv e3 ty in
      eqns1 @ eqns2 @ eqns3
  | LET (x, e1, e2) ->
      let t1 = fresh_tyvar () in
      let eqns1 = gen_equations tenv e1 t1 in
      let eqns2 = gen_equations (t_extend (x, t1) tenv) e2 ty in
      eqns1 @ eqns2
  | LETREC (f, x, e1, e2) ->
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let eqns1 = gen_equations (t_extend (f, TyFun (t1, t2)) (t_extend (x, t1) tenv)) e1 t2 in
    let eqns2 = gen_equations (t_extend (f, TyFun (t1, t2)) tenv) e2 ty in
    eqns1 @ eqns2
  | LETMREC ((f1, x1, e1), (f2, x2, e2), e3) -> 
    let t1, t2, t3 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    let env' = t_extend (f1, TyFun (t1, t3)) (t_extend (x1, t1) (t_extend (f2, TyFun (t2, t3)) (t_extend (x2, t2) tenv)))in
    let eqns1 = gen_equations env' e1 t3 in
    let eqns2 = gen_equations env' e2 t3 in
    let eqns3 = gen_equations (t_extend (f1, TyFun (t1, t3)) (t_extend (f2, TyFun (t2, t3)) tenv)) e3 ty in
    eqns1 @ eqns2 @ eqns3
  | PROC (x, e) ->
      let t1 = fresh_tyvar () in
      let t2 = fresh_tyvar () in
      [(ty, TyFun (t1, t2))] @ gen_equations (t_extend (x, t1) tenv) e t2
  | CALL (e1, e2) ->
      let t1 = fresh_tyvar () in
      let eqns1 = gen_equations tenv e1 (TyFun (t1, ty)) in
      let eqns2 = gen_equations tenv e2 t1 in
      eqns1 @ eqns2
  | PRINT e ->
      let t1 = fresh_tyvar() in let _ = gen_equations tenv e t1 in [(ty, TyUnit)]
  | SEQ (e1, e2) ->
      let t1 = fresh_tyvar () in
      let eqns1 = gen_equations tenv e1 t1 in
      let eqns2 = gen_equations tenv e2 ty in
      eqns1 @ eqns2

let rec substitute_equation eqn subst =
  map (fun (t1, t2) -> (s_apply t1 subst, s_apply t2 subst)) eqn

let rec substitute_equations eqns subst =
  map (fun eqn -> substitute_equation eqn subst) eqns

let rec is_tyvar_in_type tv ty =
  match ty with
  | TyVar tv' -> tv = tv'
  | TyFun (t1, t2) -> is_tyvar_in_type tv t1 || is_tyvar_in_type tv t2
  | TyList t -> is_tyvar_in_type tv t
  | _ -> false

let rec unify : typ_eqn -> s_t -> s_t
=fun eqns subst ->
  match eqns with
  | [] -> subst
  | (t1, t2)::tl ->
    if t1 = t2 then unify tl subst
    else 
      (match (t1, t2) with
      | (TyVar tv, _) when not (is_tyvar_in_type tv t2) -> unify tl (s_extend tv t2 subst)
      | (_, TyVar tv) when not (is_tyvar_in_type tv t1) -> unify tl (s_extend tv t1 subst)
      | (TyFun (l1, r1), TyFun (l2, r2)) -> unify ((l1, l2) :: (r1, r2) :: tl) subst   
      | (TyList l1, TyList l2) -> unify ((l1, l2) :: tl) subst        
      | _ -> raise TypeError)
  
let solve : typ_eqn -> s_t
  =fun eqns ->
    let rec solve_recursive eqns subst =
      let unified = unify eqns subst in
      if unified = subst then
        subst
      else
        solve_recursive (substitute_equation eqns unified) unified
    in
    solve_recursive eqns s_empty

let typecheck : program -> typ =
  fun exp -> 
    let new_tv = fresh_tyvar () in
    let eqns = gen_equations t_empty exp new_tv in
    let subst = solve eqns in
    let ty = s_apply new_tv subst in
      ty