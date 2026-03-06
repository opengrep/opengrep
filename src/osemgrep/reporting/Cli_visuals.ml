let hbar width =
  (* UTF-8 encoding of '─' (U+2500) repeated `width` times *)
  String.init (3 * width) (fun i ->
    char_of_int (match i mod 3 with 0 -> 0xE2 | 1 -> 0x94 | _ -> 0x80))

let logo () =
  if not (!ANSITerminal.isatty Unix.stderr) then begin
    let title    = "Opengrep" in
    let subtitle = "The open source SAST engine" in
    let w = String.length subtitle in
    let bar = hbar (w + 2) in
    Logs.app (fun m -> m "\n┌%s┐" bar);
    Logs.app (fun m -> m "│ %-*s │" w title);
    Logs.app (fun m -> m "│ %-*s │" w subtitle);
    Logs.app (fun m -> m "└%s┘\n" bar)
  end else
  let l = Ocolor_format.asprintf
{|
@{<bg_rgb(169,112,128)>  @}@{<bg_rgb(151,96,124)>  @}@{<bg_rgb(132,80,120)>  @}@{<bg_rgb(114,65,117)>  @}@{<bg_rgb(95,53,114)>  @}@{<bg_rgb(78,43,112)>  @}@{<bg_rgb(66,37,112)>  @}@{<bg_rgb(62,32,109)>  @}@{<bg_rgb(61,31,108)>  @}@{<bg_rgb(60,30,107)>                          @}@{<bg_rgb(60,29,107)>  @}@{<bg_rgb(64,32,111)>  @}@{<bg_rgb(66,34,112)>  @}@{<bg_rgb(71,38,116)>  @}@{<bg_rgb(77,44,122)>  @}@{<bg_rgb(82,49,125)>  @}@{<bg_rgb(84,54,125)>  @}@{<bg_rgb(86,56,125)>  @}@{<bg_rgb(91,63,125)>  @}@{<bg_rgb(97,74,131)>  @}@{<bg_rgb(101,84,134)>  @}@{<bg_rgb(104,94,133)>  @}@{<bg_rgb(108,106,137)>  @}@{<bg_rgb(114,118,143)>  @}@{<bg_rgb(117,125,143)>  @}@{<bg_rgb(118,130,143)>  @}
@{<bold;white>@{<bg_rgb(151,96,124)>  @}@{<bg_rgb(132,80,120)>  @}@{<bg_rgb(114,65,117)>  @}@{<bg_rgb(95,53,114)>  @}@{<bg_rgb(78,43,112)>  @}@{<bg_rgb(66,37,112)>  @}@{<bg_rgb(62,32,109)> O@}@{<bg_rgb(61,31,108)>pe@}@{<bg_rgb(60,30,107)>ngrep                     @}@{<bg_rgb(60,29,107)>  @}@{<bg_rgb(64,32,111)>  @}@{<bg_rgb(66,34,112)>  @}@{<bg_rgb(71,38,116)>  @}@{<bg_rgb(77,44,122)>  @}@{<bg_rgb(82,49,125)>  @}@{<bg_rgb(84,54,125)>  @}@{<bg_rgb(86,56,125)>  @}@{<bg_rgb(91,63,125)>  @}@{<bg_rgb(97,74,131)>  @}@{<bg_rgb(101,84,134)>  @}@{<bg_rgb(104,94,133)>  @}@{<bg_rgb(108,106,137)>  @}@{<bg_rgb(114,118,143)>  @}@{<bg_rgb(117,125,143)>  @}@{<bg_rgb(118,130,143)>  @}@{<bg_rgb(127,143,151)>  @}@}
@{<rgb(200,200,200)>@{<bg_rgb(132,80,120)>  @}@{<bg_rgb(114,65,117)>  @}@{<bg_rgb(95,53,114)>  @}@{<bg_rgb(78,43,112)>  @}@{<bg_rgb(66,37,112)>  @}@{<bg_rgb(62,32,109)>  @}@{<bg_rgb(61,31,108)> T@}@{<bg_rgb(60,30,107)>he open source SAST engine@}@{<bg_rgb(60,29,107)>  @}@{<bg_rgb(64,32,111)>  @}@{<bg_rgb(66,34,112)>  @}@{<bg_rgb(71,38,116)>  @}@{<bg_rgb(77,44,122)>  @}@{<bg_rgb(82,49,125)>  @}@{<bg_rgb(84,54,125)>  @}@{<bg_rgb(86,56,125)>  @}@{<bg_rgb(91,63,125)>  @}@{<bg_rgb(97,74,131)>  @}@{<bg_rgb(101,84,134)>  @}@{<bg_rgb(104,94,133)>  @}@{<bg_rgb(108,106,137)>  @}@{<bg_rgb(114,118,143)>  @}@{<bg_rgb(117,125,143)>  @}@{<bg_rgb(118,130,143)>  @}@{<bg_rgb(127,143,151)>  @}@{<bg_rgb(133,151,154)>  @}@}
@{<bg_rgb(114,65,117)>  @}@{<bg_rgb(95,53,114)>  @}@{<bg_rgb(78,43,112)>  @}@{<bg_rgb(66,37,112)>  @}@{<bg_rgb(62,32,109)>  @}@{<bg_rgb(61,31,108)>  @}@{<bg_rgb(60,30,107)>                          @}@{<bg_rgb(60,29,107)>  @}@{<bg_rgb(64,32,111)>  @}@{<bg_rgb(66,34,112)>  @}@{<bg_rgb(71,38,116)>  @}@{<bg_rgb(77,44,122)>  @}@{<bg_rgb(82,49,125)>  @}@{<bg_rgb(84,54,125)>  @}@{<bg_rgb(86,56,125)>  @}@{<bg_rgb(91,63,125)>  @}@{<bg_rgb(97,74,131)>  @}@{<bg_rgb(101,84,134)>  @}@{<bg_rgb(104,94,133)>  @}@{<bg_rgb(108,106,137)>  @}@{<bg_rgb(114,118,143)>  @}@{<bg_rgb(117,125,143)>  @}@{<bg_rgb(118,130,143)>  @}@{<bg_rgb(127,143,151)>  @}@{<bg_rgb(133,151,154)>  @}@{<bg_rgb(137,157,157)>  @}
|}
  in
  Logs.app (fun m -> m "%s" l)
