
let () =
  Nodejs_high_level.Os.cpu_stats ()
  |> List.iter (fun cpu ->
      let open Nodejs_high_level.Os in
      Printf.sprintf "model %s, speed: %d, times: idle: %d, sys: %d, user: %d"
        cpu.model cpu.speed cpu.times#idle cpu.times#idle cpu.times#user
      |> print_endline)
