(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

type model_output_format =
  | Scfg
  | Json

let out_testcase ~dst testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag atts name = (("", name), atts) in
  let atts = [ (("", "coversError"), "true") ] in
  let to_string v = Fmt.str "%a" Smtml.Value.pp v in
  let input v = `El (tag [] "input", [ `Data (to_string v) ]) in
  let testcase = `El (tag atts "testcase", List.map input testcase) in
  let dtd =
    {|<!DOCTYPE testcase PUBLIC "+//IDN sosy-lab.org//DTD test-format testcase 1.1//EN" "https://sosy-lab.org/test-format/testcase-1.1.dtd">|}
  in
  Xmlm.output o (`Dtd (Some dtd));
  Xmlm.output_tree Fun.id o testcase

let write_testcase =
  let cnt = ref 0 in
  fun ~dir testcase ->
    incr cnt;
    let name = Fmt.kstr Fpath.v "testcase-%d.xml" !cnt in
    let path = Fpath.append dir name in
    let* res =
      Bos.OS.File.with_oc path
        (fun chan () -> Ok (out_testcase ~dst:(`Channel chan) testcase))
        ()
    in
    res

let find_exported_name exported_names (m : Binary.modul) =
  List.find_opt
    (function
      | { Binary.name; _ } when List.mem name exported_names -> true
      | _ -> false )
    m.exports.func

let set_entry_point entry_point (m : Binary.modul) =
  (* We are checking if there's a start function *)
  if Option.is_some m.start then
    if Option.is_some entry_point then
      Fmt.error_msg
        "We don't know how to handle a custom entry point when there is a \
         start function for now. Please open a bug report."
    else Ok m
  else
    (* If there is none and we have an entry point passed in argument we search for it *)
    let* export =
      match entry_point with
      | Some entry_point -> begin
        match find_exported_name [ entry_point ] m with
        | None -> Fmt.error_msg "Entry point %s not found\n" entry_point
        | Some ep -> Ok ep
      end
      (* If we have no entry point argument then we search for common entry function names *)
      | None ->
        let possible_names = [ "main"; "_start" ] in
        begin
          match find_exported_name possible_names m with
          | Some entry_point -> Ok entry_point
          | None ->
            Fmt.error_msg "No entry point found, tried: %a\n"
              (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ", ") Fmt.string)
              possible_names
        end
    in
    (* We found an entry point, so we check its type and build a start function that put the right values on the stack,
       call the entry function and drop the results *)
    let main_id = export.id in
    if main_id >= Array.length m.func then
      Error (`Msg "can't find a main function")
    else
      let main_function = m.func.(main_id) in
      let (Bt_raw main_type) =
        match main_function with Local f -> f.type_f | Imported i -> i.desc
      in
      let default_value_of_t = function
        | Types.Num_type I32 -> Ok (Types.I32_const 0l)
        | Num_type I64 -> Ok (Types.I64_const 0L)
        | Num_type F32 -> Ok (Types.F32_const (Float32.of_float 0.))
        | Num_type F64 -> Ok (Types.F64_const (Float64.of_float 0.))
        | Ref_type (Types.Null, t) -> Ok (Types.Ref_null t)
        | Ref_type (Types.No_null, t) ->
          Error
            (`Msg
               (Fmt.str "can not create default value of type %a"
                  Types.pp_heap_type t ) )
      in
      let+ body =
        let pt, rt = snd main_type in
        let+ args = list_map (fun (_, t) -> default_value_of_t t) pt in
        let after_call =
          List.map (fun (_ : _ Types.val_type) -> Types.Drop) rt
        in
        args @ [ Types.Call (Raw main_id) ] @ after_call
      in
      let type_f : Types.binary Types.block_type =
        Types.Bt_raw (None, ([], []))
      in
      let start_code : Types.binary Types.func =
        { Types.type_f; locals = []; body; id = None }
      in
      let start_func = Runtime.Local start_code in

      (* We need to add the new start function to the funcs of the module at the next free index *)
      let func =
        Array.init
          (Array.length m.func + 1)
          (fun i -> if i = Array.length m.func then start_func else m.func.(i))
      in

      let start = Some (Array.length m.func) in
      { m with func; start }
