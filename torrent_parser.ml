open Lwt
open Cohttp_lwt_unix
open Printf
open V1_LWT

let url = Uri.make 
  ~scheme:"http" 
  ~host:"cdimage.debian.org"
  ~path:"/debian-cd/7.8.0/amd64/bt-dvd/debian-7.8.0-amd64-DVD-1.iso.torrent" 
  ()

(* we ignore the error code. *)
  
module Main (C: CONSOLE)
            (F: sig include FS with type
                page_aligned_buffer = Cstruct.t
            end)
            (CON: Conduit_mirage.S )
            = struct

    module HTTP = HTTP.Make(CON)

    type suite = {
        c : C.t;
        fs : F.t;
        con : CON.ctx;
    }

    let write_torrent suite s path =
        let fs = suite.fs
        and c = suite.c in
        F.create fs path >>= function
        | `Error e -> fail (Failure ("Error writing"))
        | `Ok _ ->
        let bytes = Cstruct.create (String.length s) in
        for i = 0 to (String.length s) - 1 do
            C.log c (sprintf "%d %c \n" i s.[i]);
            Cstruct.set_char bytes i s.[i]
        done;
        C.log c (sprintf "%d\n" (String.length s));
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
        and c = suite.c in
        let p =
        HTTP.Client.get url >>= fun (r,b) ->
        match r.Cohttp.Response.status with
            | `OK | `Code 200 ->
            Cohttp_lwt_body.to_string b >>= fun s ->
            (*write_torrent suite s path >>
            let torrent = Torrent.create_from_data s in
            Torrent.print torrent;*)
            return ()
            | _ ->
            let open Cohttp.Code in
            let open Printf in
            let s = string_of_status r.Cohttp.Response.status in
            Printf.eprintf "log: %s\n" s;
            return ()
        in p 

    let start c fs con =
        let suite = { c; fs; con} in 
        F.mkdir fs "torrents" >>
        let path = "torrents/test.torrent" in
        get_torrent suite url path >>(*
        open_torrent suite path >>= fun torrent ->
        Torrent.print torrent;*)
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
