(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.binary

let size = Units.In_bits.s32

let pp ctx = Abstract_domain.binary_pretty ctx ~size

let of_binary x = x

let to_binary x = x

let to_boolean ctx x =
  let zero = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx in
  let b = Abstract_domain.Binary_Forward.beq ~size ctx x zero in
  Abstract_boolean.not ctx b

let of_int32 ctx i =
  Abstract_domain.Binary_Forward.biconst ~size (Z.of_int32 i) ctx

let of_int ctx i = Abstract_domain.Binary_Forward.biconst ~size (Z.of_int i) ctx

let zero ctx = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx

let unknown ctx = Abstract_domain.binary_unknown ~size ctx

(* TODO: check and probably remove all `equal` functions, checks are either useless or should us `eq` instead*)
let equal i1 i2 = Abstract_domain.Binary.equal i1 i2

let eq ctx i1 i2 = Abstract_domain.Binary_Forward.beq ~size ctx i1 i2

let eqz ctx i = eq ctx (zero ctx) i

let ne ctx i1 i2 = Abstract_boolean.not ctx (eq ctx i1 i2)

(* TODO: proper handling of overflow *)

let add ctx x1 x2 =
  let flags = Operator.Flags.Biadd.no_overflow in
  Abstract_domain.Binary_Forward.biadd ~flags ~size ctx x1 x2

let sub ctx x1 x2 =
  let flags = Operator.Flags.Bisub.no_overflow in
  Abstract_domain.Binary_Forward.bisub ~flags ~size ctx x1 x2

let mul ctx x1 x2 =
  let flags = Operator.Flags.Bimul.pack ~nsw:true ~nuw:true in
  Abstract_domain.Binary_Forward.bimul ~flags ~size ctx x1 x2

let div_s = Abstract_domain.Binary_Forward.bisdiv ~size

let div_u ctx x1 x2 = Abstract_domain.Binary_Forward.biudiv ~size ctx x1 x2

let rem_s ctx x1 x2 = Abstract_domain.Binary_Forward.bismod ~size ctx x1 x2

let rem_u ctx x1 x2 = Abstract_domain.Binary_Forward.biumod ~size ctx x1 x2

let and_ ctx x1 x2 = Abstract_domain.Binary_Forward.band ~size ctx x1 x2

let or_ ctx x1 x2 = Abstract_domain.Binary_Forward.bor ~size ctx x1 x2

let le_s ctx x1 x2 = Abstract_domain.Binary_Forward.bisle ~size ctx x1 x2

let le_u ctx x1 x2 = Abstract_domain.Binary_Forward.biule ~size ctx x1 x2

let lt_s ctx x1 x2 =
  let le = le_s ctx x1 x2 in
  let neq = Abstract_boolean.not ctx (eq ctx x1 x2) in
  Abstract_boolean.and_ ctx le neq

let lt_u ctx x1 x2 =
  let le = le_u ctx x1 x2 in
  let neq = Abstract_boolean.not ctx (eq ctx x1 x2) in
  Abstract_boolean.and_ ctx le neq

let of_boolean ctx boolean =
  let bool_lattice = Abstract_domain.query_boolean ctx boolean in
  let truthy, falsy = Codex.Lattices.Quadrivalent.to_bools bool_lattice in
  match (truthy, falsy) with
  | false, true -> (ctx, of_int ctx 0)
  | true, false -> (ctx, of_int ctx 1)
  | false, false -> assert false
  | true, true -> (
    let i = unknown ctx in
    let ge0 = Abstract_boolean.not ctx (lt_s ctx i (zero ctx)) in
    let le1 = le_s ctx i (of_int ctx 1) in
    match Abstract_domain.assume ctx (Abstract_boolean.and_ ctx ge0 le1) with
    | Some ctx -> (ctx, i)
    | None -> assert false )
