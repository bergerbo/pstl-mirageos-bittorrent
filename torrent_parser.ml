open Lwt
open Cohttp_lwt_unix
open Printf
open V1_LWT


module Main (C: CONSOLE)
            (F: sig include FS with type
                page_aligned_buffer = Cstruct.t
            end)
            (RES:Resolver_lwt.S)
            (CON: Conduit_mirage.S ) = struct

    module HTTP = HTTP.Make(CON)

    type suite = {
        c : C.t;
        fs : F.t;
        res : Resolver_lwt.t;
        ctx : CON.ctx;
    }
    let url = Uri.of_string
"http://cdimage.debian.org/debian-cd/current-live/amd64/bt-hybrid/debian-live-7.8.0-amd64-standard.iso.torrent"

    let write_torrent suite s path =
        let fs = suite.fs
        and c = suite.c in
        F.create fs path >>= function
        | `Error e -> fail (Failure ("Error creating file"))
        | `Ok _ ->
        let bytes = Cstruct.create (String.length s) in
        for i = 0 to (String.length s) - 1 do
            Cstruct.set_char bytes i s.[i]
        done;
        (F.write fs path 0 bytes) >>= function
        | `Error e -> fail (Failure ("Error writing"))
        | `Ok _ -> C.log c "Done"; return ()

    let open_torrent suite path =
        let fs = suite.fs in
        F.size fs path >>= function
        | `Error e -> fail(Failure ("Error getting size"))
        | `Ok size ->
        F.read fs path 0 (Int64.to_int size) >>= function
        | `Error e -> fail(Failure ("Error reading"))
        | `Ok pages ->
        let data = Cstruct.copyv pages in
        return (Torrent.create_from_data data)


    let get_torrent suite url path=
        let fs = suite.fs
        and c = suite.c 
        and ctx = suite.ctx 
        and resolver = suite.res in
        let ctx = { HTTP.Net_IO.resolver; ctx} in
        HTTP.Client.get ~ctx  url >>= fun (r,b) ->
        match r.Cohttp.Response.status with
        | `OK | `Code 200 ->
        Cohttp_lwt_body.to_string b >>= fun s ->
        write_torrent suite s path >>
        let torrent = Torrent.create_from_data s in
        Torrent.print torrent;
        return ()
        | _ ->
        let open Cohttp.Code in
        let open Printf in
        let s = string_of_status r.Cohttp.Response.status in
        Printf.eprintf "log: %s\n" s;
        return ()

    let start c fs res ctx =
        let suite = { c; fs; res;  ctx} in 
        F.mkdir fs "torrents" >>
        let path = "torrents/test.torrent" in
        get_torrent suite url path >>
        open_torrent suite path >>= fun torrent ->
        Torrent.print torrent;
        return ()

    (*
    let get_torrent =
        let output = "test.torrent" in
        let p =
        Client.get url >>= fun (r,b) ->
        match r.Cohttp.Response.status with
            | `OK | `Code 200 ->
            Cohttp_lwt_body.to_string b >>= fun s ->
            Lwt_io.open_file Lwt_io.Output output >>= fun oc ->
            Lwt_io.write oc s >>= fun () ->
            Lwt_io.close oc >>= fun () ->
            let torrent = Torrent.create_from_file output in
            Torrent.print torrent;
            return ()
            | _ ->
            let open Cohttp.Code in
            let open Printf in
            let s = string_of_status r.Cohttp.Response.status in
            Printf.eprintf "log: %s\n" s;
            return ()
        in Lwt_main.run p
    *)
end
