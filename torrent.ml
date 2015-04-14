type file = {
    path : string option list;
    length : int option;
}


type t = {
    encoded : string;
    announce : string option;
    name : string option;
    length : int option;
    piece_length : int option;
    files : file list;
    pieces : string option;
}


let rec match_field decoded field =
    match decoded with
    | Bencode.Dict list ->
          ( match list with
            | [] -> None
            | (key,value)::tl ->
                    if (String.compare key field) == 0 then
                            Some value
                    else
                        let v = match_field value field in
                        match v with
                        | None -> match_field (Bencode.Dict tl) field
                        | x -> x )

    | Bencode.List list ->
          ( match list with
            | [] -> None
            | hd::tl ->
                let v = match_field hd field in
                match v with
                | None -> match_field (Bencode.List tl) field
                | x -> x )

    | Bencode.Integer i -> None

    | Bencode.String s -> None


let apply f opt  =
    match opt with
    | None -> None
    | Some x -> f x

let none_to o def =
    match o with
    | None -> def
    | Some x -> x

let rec files_from_list list files =
    match list with
    | [] -> files
    | hd::tl ->
            let p = apply (Bencode.as_list) (match_field hd "path")
            and length = apply (Bencode.as_int) (match_field hd "length") in
            let path = 
                List.map (fun s -> Bencode.as_string s) (none_to p []) in
            let file = { path; length } in
            files_from_list tl files@[file]


let create_from_decoded d =
    let encoded = Bencode.encode_to_string d in
    let announce = apply (Bencode.as_string) (match_field d "announce")
    and name = apply (Bencode.as_string) (match_field d "name")
    and length = apply (Bencode.as_int) (match_field d "length")
    and piece_length = apply (Bencode.as_int) (match_field d "piece length")
    and pieces = apply (Bencode.as_string) (match_field d "pieces") 
    and decoded_files = apply (Bencode.as_list) (match_field d "files") in
    let files =
        files_from_list (none_to decoded_files []) [] in
    { encoded; announce; name; length;
      piece_length; files;  pieces }

let create_from_file filepath =
    let d = Bencode.decode (`File_path  filepath ) in
    create_from_decoded d

let create_from_data data =
    let d = Bencode.decode (`String  data ) in
    create_from_decoded d

let name torrent =
    match torrent.name with
    | None -> ""
    | Some str -> str

let announce torrent =
    match torrent.announce with
    | None -> ""
    | Some str -> str

let encoded_info torrent =
    let d = Bencode.decode (`String torrent.encoded) in
    let info = match_field d "info" in
    match info with 
    | None -> ""
    | Some d -> Bencode.encode_to_string d

let length torrent =
    let rec files_length (f: file list) length =
        match f with
        | [] -> length
        | hd::tl ->
                let l = none_to hd.length 0 in
                files_length tl (length + l)
    in
    match torrent.length with
    | None -> files_length torrent.files 0
    | Some l -> l

let encoded torrent =
    torrent.encoded

let print t = 
    Printf.printf "announce : %s\n" (none_to t.announce "");
    Printf.printf "name :%s\n" (none_to t.name "");
    Printf.printf "pieces length : %d\n" (none_to t.piece_length 0);
    List.iter
        (fun file -> 
            Printf.printf "file : %s %d\n" 
            (String.concat "/" (List.map (fun s -> none_to s "") file.path ))
            (none_to file.length 0))
        t.files
