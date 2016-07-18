
module Fs = struct

  let read_file filename =
    let t, w = Lwt.task () in
    Nodejs_high_level.Fs.read_file_async filename (fun err content -> Lwt.wakeup w content);
    t

end

