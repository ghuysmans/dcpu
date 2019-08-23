type reg =
  | A | B | C
  | X | Y | Z
  | I | J

let encode_reg = function
  | A -> 0
  | B -> 1
  | C -> 2
  | X -> 3
  | Y -> 4
  | Z -> 5
  | I -> 6
  | J -> 7

(* defined here to avoid forgetting cases in encode* functions *)
type value = [
  | `Register of reg
  | `Indirect of reg
  | `BasedIndirect of reg * int
  | `Top
  | `Pick of int
  | `SP
  | `PC
  | `EX
  | `Direct of int
  | `Immediate of int
  | `SmallImmediate of int (* never written *)
]

let word x =
  if x >= 0 && x < 0x10000 then
    x
  else
    raise (Invalid_argument (string_of_int x ^ " is not a word"))

let small x =
  if x >= -1 && x <= 30 then
    x
  else
    raise (Invalid_argument (string_of_int x ^ " is not in [-1;30]"))

let encode_value : value -> int * int list = function
  | `Register r -> encode_reg r, []
  | `Indirect r -> 8 + encode_reg r, []
  | `BasedIndirect (r, i) -> 16 + encode_reg r, [word i]
  | `Top -> 0x19, []
  | `Pick i -> 0x1a, [word i]
  | `SP -> 0x1b, []
  | `PC -> 0x1c, []
  | `EX -> 0x1d, []
  | `Direct a -> 0x1e, [word a]
  | `Immediate v -> 0x1f, [word v]
  | `SmallImmediate v -> 0x21 + small v, []

(* Push and Pop could (should) have been handled more efficiently
 * (using two names for the same `PushPop value, for example),
 * but this exhibits polymorphism. *)

type rvalue = [
  | `Pop
  | value
]

let encode_rvalue : rvalue -> int * int list = function
  | `Pop -> 0x18, []
  | #value as v -> encode_value v

type lvalue = [
  | `Push
  | value
]

let encode_lvalue : lvalue -> int * int list = function
  | `Push -> 0x18, []
  | #value as v -> encode_value v

type basic_op = [
  | `Set
  | `Add
  | `Sub
  | `Mul
  | `Mli
  | `Div
  | `Dvi
  | `Mod
  | `Mdi
  | `And
  | `Bor
  | `Xor
  | `Shr
  | `Asr
  | `Shl
  | `Ifb
  | `Ifc
  | `Ife
  | `Ifn
  | `Ifg
  | `Ifa
  | `Ifl
  | `Ifu
  | `Adx
  | `Sbx
  | `Sti
  | `Std
]

let encode_basic_op : basic_op -> int = function
  | `Set -> 0x01
  | `Add -> 0x02
  | `Sub -> 0x03
  | `Mul -> 0x04
  | `Mli -> 0x05
  | `Div -> 0x06
  | `Dvi -> 0x07
  | `Mod -> 0x08
  | `Mdi -> 0x09
  | `And -> 0x0a
  | `Bor -> 0x0b
  | `Xor -> 0x0c
  | `Shr -> 0x0d
  | `Asr -> 0x0e
  | `Shl -> 0x0f
  | `Ifb -> 0x10
  | `Ifc -> 0x11
  | `Ife -> 0x12
  | `Ifn -> 0x13
  | `Ifg -> 0x14
  | `Ifa -> 0x15
  | `Ifl -> 0x16
  | `Ifu -> 0x17
  | `Adx -> 0x1a
  | `Sbx -> 0x1b
  | `Sti -> 0x1e
  | `Std -> 0x1f

let assemble_basic (op, l, r) =
  let a, n = encode_rvalue r in
  let b, n' = encode_lvalue l in
  [(a lsl 10) lor (b lsl 5) lor encode_basic_op op] @ n' @ n

type special_op = [
  | `Jsr
  | `Int
  | `Iag
  | `Ias
  | `Rfi
  | `Iaq
  | `Hwn
  | `Hwq
  | `Hwi
]

let encode_special_op : special_op -> int = function
  | `Jsr -> 0x01
  | `Int -> 0x08
  | `Iag -> 0x09
  | `Ias -> 0x0a
  | `Rfi -> 0x0b
  | `Iaq -> 0x0c
  | `Hwn -> 0x10
  | `Hwq -> 0x11
  | `Hwi -> 0x12

let assemble_special (op, x) =
  let a, n = encode_rvalue x in
  [(a lsl 10) lor (encode_special_op op lsl 5)] @ n

type op = [
  | basic_op
  | special_op
]

let assemble ((op, l, r) : op * lvalue * rvalue) = match op with
  | #basic_op as b -> assemble_basic (b, l, r)
  | #special_op as s -> assemble_special (s, r)


let dump p =
  List.iter (fun x -> Printf.printf "%04x " x) p;
  print_newline ()


let%expect_test _ =
  (* TODO recursive tree structure for (hidden) labels! *)
  let prog = [`Int, `EX, `SmallImmediate 3;
              `Set, `Register A, `Register I;
              `Add, `Register A, `SmallImmediate 2;
              `Set, `Register I, `Register A] in
  List.map assemble prog |> List.concat |> dump;
  [%expect {|
  9100 1801 8c02 00c1
  |}]

(*
let l : [< lvalue] list = [`Push; `SP]
let l' = [`Push; `SP]
let m : [> `x] list = [`y]
let nope : [< `x] list = [`x; `y]
*)
