open Core

let sorted_files_in_local_dir dir_name =
  let files_in_dir =
    Sys_unix.readdir (Filename.concat Filename.current_dir_name dir_name)
  in
  Array.sort files_in_dir ~compare:[%compare: string];
  files_in_dir
;;
