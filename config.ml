open Mirage

let net = `Direct
    (*try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | _ -> `Socket
    with Not_found -> `Direct*)

let dhcp = `Static
    (*try match Sys.getenv "DHCP" with
    | "0" -> `Static
    | _ -> `Dhcp
    with Not_found -> `Dhcp*)

let stack console =
    match net, dhcp with
    | `Direct, `Dhcp -> direct_stackv4_with_dhcp console tap0
    | `Direct, `Static -> direct_stackv4_with_default_ipv4 console tap0
    | `Socket, _ -> socket_stackv4 console [Ipaddr.V4.any]

let main =
    foreign "Torrent_parser.Main" @@ console @-> fs  @-> conduit @-> job

let () =
    add_to_ocamlfind_libraries ["mirage-http"];
    add_to_opam_packages ["mirage-http"];
    let sv4 = stack default_console in
    let conduit = conduit_direct sv4 in
    let fat_fs = fat(block_of_file "disk.img") in 
    let job = [main $ default_console $ fat_fs $ conduit ] in
    register "main" job
