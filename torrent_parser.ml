open Lwt
open Cohttp_lwt_unix

let url = Uri.make 
  ~scheme:"http" 
  ~host:"cdimage.debian.org"
  ~path:"/debian-cd/7.8.0/amd64/bt-dvd/debian-7.8.0-amd64-DVD-1.iso.torrent" 
  ()

(* we ignore the error code. *)
  
let _ =
  let output = Sys.argv.(1) in
  let p =
    Client.get url >>= fun (r,b) ->
    match r.Cohttp.Response.status with
    | `OK | `Code 200 ->
       Cohttp_lwt_body.to_string b >>= fun s ->
       Lwt_io.open_file Lwt_io.Output output >>= fun oc ->
       Lwt_io.write oc s >>= fun () ->
       Lwt_io.close oc >>= fun () ->
           let torrent = Torrent.create_from_file output in
           Printf.printf "torrent name = %s\n" (Torrent.name torrent);
           return ()
    | _ ->
       let open Cohttp.Code in
       let open Printf in
       let s = string_of_status r.Cohttp.Response.status in
       Printf.eprintf "log: %s\n" s;
       return ()
  in Lwt_main.run p
