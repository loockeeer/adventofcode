let download_input dest cookie year day =
  Sys.command (Printf.sprintf "wget -O \"%s\" --header=\"Cookie: %s\" https://adventofcode.com/%d/day/%d/input > /dev/null 2>&1" dest cookie year day);;
