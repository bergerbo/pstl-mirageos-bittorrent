(* Generated by Mirage (Thu, 5 Mar 2015 21:04:18 GMT). *)

open Lwt

let _ = Printexc.record_backtrace true

module Console = Console_unix

let console1 () =
  Console.connect "0"

module M2 = Unikernel.Main(Console)

let t2 = console1

let net_tap0 () =
  Netif.connect "tap0"

module M1 = Unikernel.Main(Console)(Netif)

let t1 () =
  console1 () >>= function
  | `Error e -> fail (Failure "console1")
  | `Ok console1 ->
  net_tap0 () >>= function
  | `Error e -> fail (Failure "net_tap0")
  | `Ok net_tap0 ->
  M1.start console1 net_tap0

let () =
  OS.Main.run (join [t1 ()])