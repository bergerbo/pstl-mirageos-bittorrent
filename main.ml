(* Generated by Mirage (Wed, 11 Mar 2015 17:58:29 GMT). *)

open Lwt

let _ = Printexc.record_backtrace true

module Console = Console_unix

let console1 () =
  Console.connect "0"

module M2 = Torrent_parser.Main(Console)

let t2 = console1

let block1 () =
  Block.connect "/home/rockinbobo/lmd/master/pstl/disk.img"

module M1 = Torrent_parser.Main(Console)(Block)

let t1 () =
  block1 () >>= function
  | `Error e -> fail (Failure "block1")
  | `Ok block1 ->
  console1 () >>= function
  | `Error e -> fail (Failure "console1")
  | `Ok console1 ->
  M1.start console1 block1

let () =
  OS.Main.run (join [t1 ()])