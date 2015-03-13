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
  
module Main (C: CONSOLE) (B: BLOCK) = struct

    let write c s block =
        B.get_info block >>= fun info ->
        let nbp = ((String.length s) / info.sector_size ) + 1 in
        let page = Io_page.(to_cstruct (get nbp)) in
        let page = Cstruct.sub page 0 (nbp * info.sector_size) in
        for i = 0 to (String.length s)  - 1 do
            C.log c (sprintf "%d  %c \n" i s.[i]);
            Cstruct.set_char page i s.[i]
        done;
        C.log c (sprintf "%d\n" (String.length s));
        (B.write block 0L [page]) >>= function
        | `Error e -> fail (Failure ("Error writing"))
        | `Ok _ -> C.log c "Done"; return ()

    let start c b =
        let torrent = Torrent.create_from_file "./test.torrent" in
        C.log c "torrent created";
        write c (Torrent.encoded torrent) b >>
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
