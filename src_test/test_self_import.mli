type t = [`OptionA | `OptionB]

module type S = sig
  val test : unit -> string
end

val validate_option : t -> unit
val validate_module_type : (module S) -> unit
