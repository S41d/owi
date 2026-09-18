(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { content : string
  ; dropped : bool
  }

let value { content; dropped } = if dropped then "" else content

let size { content; _ } = String.length content

let drop { content; _ } = { content; dropped = true }

let of_string str = { content = str; dropped = false }

let to_string { content; dropped } = if dropped then None else Some content
