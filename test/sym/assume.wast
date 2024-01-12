(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))
  (import "symbolic" "assume_positive_i32" (func $positive_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start
    (local $x i32)
    (local $y i32)
    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))
    (call $assume_i32 (local.get $x))
    (call $positive_i32 (local.get $y))
    (call $assert_i32 (local.get $y))
    (if (i32.gt_s (local.get $x) (local.get $y))
      (then unreachable)))

  (start $start)
)
