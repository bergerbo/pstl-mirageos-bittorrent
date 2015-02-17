open Core.Std
open Async.Std

let url_setup = Uri.make 
    ~host:"http://cdimage.debian.org/"
    ~path:"debian-cd/7.8.0/amd64/bt-dvd/debian-7.8.0-amd64-DVD-1.iso.torrent"

let url = url_setup()

let req = Cohttp.Request.make ~meth:`GET url


