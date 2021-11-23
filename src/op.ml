let i32_clz = Ocaml_intrinsics.Int32.count_leading_zeros

let i64_clz = Ocaml_intrinsics.Int64.count_leading_zeros

let i32_ctz = Ocaml_intrinsics.Int32.count_trailing_zeros

let i64_ctz = Ocaml_intrinsics.Int64.count_trailing_zeros

(* Taken from Base *)
let i64_popcnt =
  let open Int64 in
  let ( + ) = add in
  let ( - ) = sub in
  let ( * ) = mul in
  let ( lsr ) = shift_right_logical in
  let ( land ) = logand in
  let m1 = 0x5555555555555555L in
  (* 0b01010101... *)
  let m2 = 0x3333333333333333L in
  (* 0b00110011... *)
  let m4 = 0x0f0f0f0f0f0f0f0fL in
  (* 0b00001111... *)
  let h01 = 0x0101010101010101L in
  (* 1 bit set per byte *)
  fun [@inline] x ->
    (* gather the bit count for every pair of bits *)
    let x = x - ((x lsr 1) land m1) in
    (* gather the bit count for every 4 bits *)
    let x = (x land m2) + ((x lsr 2) land m2) in
    (* gather the bit count for every byte *)
    let x = (x + (x lsr 4)) land m4 in
    (* sum the bit counts in the top byte and shift it down *)
    to_int ((x * h01) lsr 56)

(* Taken from Base *)
let i32_popcnt =
  (* On 64-bit systems, this is faster than implementing using [int32] arithmetic. *)
  let mask = 0xffff_ffffL in
  fun [@inline] x -> i64_popcnt (Int64.logand (Int64.of_int32 x) mask)
