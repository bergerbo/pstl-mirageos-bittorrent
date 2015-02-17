open Core.Std
open Async.Std

let url_setup = Uri.make ~scheme:"http" ~host:"cdimage.debian.org"
    ~path:"/debian-cd/7.8.0/amd64/bt-dvd/debian-7.8.0-amd64-DVD-1.iso.torrent"

let url = url_setup()

let get = Cohttp_lwt_unix.Client.get url


