;; Test that traps are preserved even in instructions which might otherwise
;; be dead-code-eliminated. These functions all perform an operation and
;; discard its return value.

(module
  (func (export "no_dce.i32.div_s") (param $x i32) (param $y i32)
    (drop (i32.div_s (local.get $x) (local.get $y))))
  (func (export "no_dce.i32.div_u") (param $x i32) (param $y i32)
    (drop (i32.div_u (local.get $x) (local.get $y))))
  (func (export "no_dce.i64.div_s") (param $x i64) (param $y i64)
    (drop (i64.div_s (local.get $x) (local.get $y))))
  (func (export "no_dce.i64.div_u") (param $x i64) (param $y i64)
    (drop (i64.div_u (local.get $x) (local.get $y))))
)

(assert_trap (invoke "no_dce.i32.div_s" (i32.const 1) (i32.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i32.div_u" (i32.const 1) (i32.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i64.div_s" (i64.const 1) (i64.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i64.div_u" (i64.const 1) (i64.const 0)) "integer divide by zero")
;; TODO (assert_trap (invoke "no_dce.i32.div_s" (i32.const 0x80000000) (i32.const -1)) "integer overflow")
;; TODO (assert_trap (invoke "no_dce.i64.div_s" (i64.const 0x8000000000000000) (i64.const -1)) "integer overflow")

(module
  (func (export "no_dce.i32.rem_s") (param $x i32) (param $y i32)
    (drop (i32.rem_s (local.get $x) (local.get $y))))
  (func (export "no_dce.i32.rem_u") (param $x i32) (param $y i32)
    (drop (i32.rem_u (local.get $x) (local.get $y))))
  (func (export "no_dce.i64.rem_s") (param $x i64) (param $y i64)
    (drop (i64.rem_s (local.get $x) (local.get $y))))
  (func (export "no_dce.i64.rem_u") (param $x i64) (param $y i64)
    (drop (i64.rem_u (local.get $x) (local.get $y))))
)

(assert_trap (invoke "no_dce.i32.rem_s" (i32.const 1) (i32.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i32.rem_u" (i32.const 1) (i32.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i64.rem_s" (i64.const 1) (i64.const 0)) "integer divide by zero")
(assert_trap (invoke "no_dce.i64.rem_u" (i64.const 1) (i64.const 0)) "integer divide by zero")

(module
  (func (export "no_dce.i32.trunc_f32_s") (param $x f32) (drop (i32.trunc_f32_s (local.get $x))))
  (func (export "no_dce.i32.trunc_f32_u") (param $x f32) (drop (i32.trunc_f32_u (local.get $x))))
  (func (export "no_dce.i32.trunc_f64_s") (param $x f64) (drop (i32.trunc_f64_s (local.get $x))))
  (func (export "no_dce.i32.trunc_f64_u") (param $x f64) (drop (i32.trunc_f64_u (local.get $x))))
  (func (export "no_dce.i64.trunc_f32_s") (param $x f32) (drop (i64.trunc_f32_s (local.get $x))))
  (func (export "no_dce.i64.trunc_f32_u") (param $x f32) (drop (i64.trunc_f32_u (local.get $x))))
  (func (export "no_dce.i64.trunc_f64_s") (param $x f64) (drop (i64.trunc_f64_s (local.get $x))))
  (func (export "no_dce.i64.trunc_f64_u") (param $x f64) (drop (i64.trunc_f64_u (local.get $x))))
)

(assert_trap (invoke "no_dce.i32.trunc_f32_s" (f32.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i32.trunc_f32_u" (f32.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i32.trunc_f64_s" (f64.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i32.trunc_f64_u" (f64.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i64.trunc_f32_s" (f32.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i64.trunc_f32_u" (f32.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i64.trunc_f64_s" (f64.const nan)) "invalid conversion to integer")
(assert_trap (invoke "no_dce.i64.trunc_f64_u" (f64.const nan)) "invalid conversion to integer")

(module
    (memory 1)

    (func (export "no_dce.i32.load") (param $i i32) (drop (i32.load (local.get $i))))
    (func (export "no_dce.i32.load16_s") (param $i i32) (drop (i32.load16_s (local.get $i))))
    (func (export "no_dce.i32.load16_u") (param $i i32) (drop (i32.load16_u (local.get $i))))
    (func (export "no_dce.i32.load8_s") (param $i i32) (drop (i32.load8_s (local.get $i))))
    (func (export "no_dce.i32.load8_u") (param $i i32) (drop (i32.load8_u (local.get $i))))
    (func (export "no_dce.i64.load") (param $i i32) (drop (i64.load (local.get $i))))
    (func (export "no_dce.i64.load32_s") (param $i i32) (drop (i64.load32_s (local.get $i))))
    (func (export "no_dce.i64.load32_u") (param $i i32) (drop (i64.load32_u (local.get $i))))
    (func (export "no_dce.i64.load16_s") (param $i i32) (drop (i64.load16_s (local.get $i))))
    (func (export "no_dce.i64.load16_u") (param $i i32) (drop (i64.load16_u (local.get $i))))
    (func (export "no_dce.i64.load8_s") (param $i i32) (drop (i64.load8_s (local.get $i))))
    (func (export "no_dce.i64.load8_u") (param $i i32) (drop (i64.load8_u (local.get $i))))
    (func (export "no_dce.f32.load") (param $i i32) (drop (f32.load (local.get $i))))
    (func (export "no_dce.f64.load") (param $i i32) (drop (f64.load (local.get $i))))
)

(assert_trap (invoke "no_dce.i32.load" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i32.load16_s" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i32.load16_u" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i32.load8_s" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i32.load8_u" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load32_s" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load32_u" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load16_s" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load16_u" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load8_s" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.i64.load8_u" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.f32.load" (i32.const 65536)) "out of bounds memory access")
(assert_trap (invoke "no_dce.f64.load" (i32.const 65536)) "out of bounds memory access")
