module CaseCons where


--ERROR:
go l = case l of
  [] -> 0
  h : t -> h + 1
