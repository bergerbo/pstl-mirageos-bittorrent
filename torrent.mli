type t
val create_from_file : string -> t
val create_from_data : string -> t
val name : t -> string
val announce : t -> string
val encoded_info : t -> string
val length : t -> int
val encoded : t -> string
val print : t -> unit
