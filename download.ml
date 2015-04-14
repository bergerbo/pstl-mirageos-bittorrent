open Lwt
open V1_LWT

module Make (CON: Conduit_mirage.S)
            (RES: Resolver_lwt.S)
            (F: sig include FS with type
                page_aligned_buffer = Cstruct.t
            end) = struct

    module HTTP = HTTP.Make(CON)

    type peer = {
        ip : string;
    }

    type t = {
        torrent : Torrent.t;
        mutable finished : bool;
        mutable running : bool;
        mutable tracker_updaters : (unit Lwt.t) list;
        mutable peers : peer list;
        mutable peer_connections : (unit Lwt.t) list;
    }

    let create torrent =
        {
            torrent;
            finished = false;
            running = false;
            tracker_updaters = [];
            peers = [];
            peer_connections = [];
        }

    let parse_peers response =
         return []

    let fuse_peers p1 p2 =
        let rec contains p l =
            match l with
            | [] -> false
            | hd::tl -> if (p.ip == hd.ip) then true else (contains p tl)
        in
        let rec fuse p1 p2 =
            match p1 with
            | [] -> p2
            | hd::tl -> if (contains hd p2) then
                   (fuse tl p2)
                else
                    (fuse tl (hd::p2))

        in fuse p1 p2

    let add_peers peers download =
        download.peers <- (fuse_peers peers download.peers)

    let build_uri download =
        let uri = Uri.of_string (Torrent.announce download.torrent) in
        let binfo = Torrent.encoded_info download.torrent in
        let shainfo = Sha1.to_bin (Sha1.string binfo) in
        let id = "alkj8Fix4hU78s1Qo08h"
        and p = "6881"
        and up = "0"
        and down = "0"
        and left = string_of_int (Torrent.length download.torrent) in
        let open Uri in
        let params = 
            [("info_hash", (pct_encode ~component:`Query_value shainfo));
            ("peer_id", (pct_encode ~component:`Query_value id));
            ("port", p);
            ("uploaded", up);
            ("downloaded",down);
            ("left", left)] in
        let uri = add_query_params' uri params in
        uri


    let tracker_updater ctx resolver download =
        let ctx = { HTTP.Net_IO.resolver; ctx} in
        let uri = build_uri download in
        HTTP.Client.get ~ctx uri >>=
        fun (r,b) -> match r.Cohttp.Response.status with
        | `OK | `Code 200 ->
        Cohttp_lwt_body.to_string b >>= fun s ->
        parse_peers s >>= fun peers ->
            add_peers peers download;
            return ()
        | _ ->
        let open Cohttp.Code in
        let open Printf in
        let s = string_of_status r.Cohttp.Response.status in
        Printf.eprintf "log: %s\n" s;
        return ()

    let start ctx res t =
        if t.running == false then
        t.running <- true;
        t.tracker_updaters <- [(tracker_updater ctx res t)]
end
