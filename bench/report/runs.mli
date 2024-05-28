type t

val empty : t

val add : Run.t -> t -> t

val count_nothing : t -> int

val count_reached : t -> int

val count_timeout : t -> int

val count_other : t -> int

val count_killed : t -> int

val keep_nothing : t -> t

val keep_reached : t -> t

val keep_timeout : t -> t

val keep_other : t -> t

val keep_killed : t -> t

val min_clock : t -> float

val max_clock : t -> float

val to_distribution : max_time:int -> t -> float list

val pp_quick_results : Format.formatter -> t -> unit

val map : (Run.t -> 'a) -> t -> 'a list
