-- https://github.com/jeremyrsellars/cecil/issues/1
  select
    r.*
  from re r
  where
    r.r > ''
  order by
    r.a,
    r.b
