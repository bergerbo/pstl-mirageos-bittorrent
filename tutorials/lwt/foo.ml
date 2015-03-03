open V1_LWT
open Lwt
open OS


let map f mb =
    let nmb = Lwt_mvar.create_empty () in
    let rec maprec () =
            let open Lwt_mvar in
            take mb >>=
            f  >>=
            fun fm -> put nmb fm >>
            maprec ()
        in
        let t = maprec () in
        nmb


let split mb =
    let mb_a = Lwt_mvar.create_empty ()
    and mb_b = Lwt_mvar.create_empty ()
    in
    let rec splitrec () =
        let open Lwt_mvar in
        take mb >>= fun (a,b) ->
        join [ put mb_a a; put mb_b b ] >>
        splitrec ()
    in
    let t = splitrec() in
    (mb_a, mb_b)


let filter f mb =
    let nmb = Lwt_mvar.create_empty () in
    let rec filtrec () =
        let open Lwt_mvar in
            take mb >>= fun m ->
            f m     >>= fun b ->
            (if b then 
                put nmb m >> filtrec ()
                else filtrec () )
    in 
    let t = filtrec () in
    nmb

let read_line () =
    return (String.make (Random.int 20) 'a')

let uppercase s =
    return (String.uppercase s )

let wait_string s =
    let l = float_of_int (String.length s) in
        Time.sleep l >>
        return s

let add_mult (a,b) =
    return (a+b, a*b)

module Main (C : CONSOLE) = struct

    let print c i =
        C.log c (Printf.sprintf "%d" i);
        return i

    let odd i =
        return ((i mod 2) = 1)

    let print_odd c i =
        C.log c "Odd";
        return i

    let start c =
        let m_in = Lwt_mvar.create_empty () in
        let (m_add, m_mult) = m_in |> map add_mult |> split in
        let print_filter mb =
            mb |> map (print c) |> filter odd |> map (print_odd c)
        in
        let rec input () =
            Time.sleep 1. >>
            Lwt_mvar.put m_in (Random.int 1000, Random.int 1000) >>
            input ()
        in

        let rec process mb =
            Lwt_mvar.take (print_filter mb) >>
            process mb
    in
    input ()  <&> process m_add <&> process m_mult

end

module MainString (C : CONSOLE) = struct

    let start c =
        let m_in = Lwt_mvar.create_empty () in
        let m_out = m_in |> map wait_string |> map uppercase in

        let rec input () =
            read_line () >>= fun s ->
            Lwt_mvar.put m_in s >>
            input ()
        in

        let rec process () =
            Lwt_mvar.take m_out >>= fun s ->
            Lwt.return (C.log c s) >>
            process ()
    in
    input ()  <&> process ()

end


(* Mailboxes    
            *)



(*  Sleep & join
   join [
    (Time.sleep 1.0 >>= (fun () -> C.log c "Heads"; return ()));
    (Time.sleep 2.0 >>= (fun () -> C.log c "Tails"; return ()));
    ] >>= (fun () -> C.log c "Finished"; return ())
    *)



(* Timeout
        let timeout f t =
            Time.sleep f >>
            match state t with
            | Return v -> return (Some v)
            | _        -> cancel t; return None
        in


        let t = Time.sleep (Random.float 3.0) >> return "Heads" in
            timeout 2.0 t >>= fun v ->
            C.log c (match v with None -> "cancelled" | Some v -> v);
            C.log c "Finished";
            return ()
            *)
