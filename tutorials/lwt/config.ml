open Mirage

let () =
    let main = foreign "Foo.Main" (console @-> job) in
    register "Foo.Main" [
         main $ default_console
    ]
