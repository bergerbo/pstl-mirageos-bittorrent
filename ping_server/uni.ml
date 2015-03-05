open Lwt
open V1_LWT

let ip_addr = "127.0.0.1"
let netmask = "255.255.255.0"
let gateways = ["10.0.0.1"]

module Main (C:CONSOLE) (N:NETWORK) = struct

    module Eth = Ethif.Make(N)
    module Ip = Ipv4.Make(Eth)

    let or_error c msg f a =
        f a >>= function
        | `Ok r -> return r
        | `Error e -> fail (Failure  ("Error : "^msg))

    let start c n =
        or_error c "Ethif" Eth.connect n >>= fun e ->
        or_error c "Ipv4"  Ip.connect e >>= fun i ->

        Ip.set_ip i (Ipaddr.V4.of_string_exn ip_addr) >>
        Ip.set_ip_netmask i (Ipaddr.V4.of_string_exn netmask) >>
        Ip.set_ip_gateways i (List.map Ipaddr.V4.of_string_exn gateways) >>

        let handle_echo = fun ~src ~dst data ->
            C.log_s c (Printf.sprintf "%s > %s"
                         (Ipaddr.V4.to_string src) (Ipaddr.V4.to_string dst))

        in
        N.listen n
            (Eth.input
                ~arpv4:(Ip.input_arpv4 i)
                ~ipv4:(Ip.input
                        ~tcp: (handle_echo)
                        ~udp: (handle_echo)
                        ~default:(fun ~proto ~src ~dst data ->
                            C.log_s c "Default")
                        i
                        )
                ~ipv6:(fun buf -> return (C.log c "IPv6"))
            e)
end

