open V1_LWT
module Make (CON: Conduit_mirage.S)
            (RES: Resolver_lwt.S)
            (F: sig include FS with type
                page_aligned_buffer = Cstruct.t
            end)  : sig

    type peer

    type t

    val create : Torrent.t -> t

    val start : CON.ctx -> Resolver_lwt.t -> t -> unit
end
