select
    count(cv.code_value)
from
  location l
inner join
  location_group lg
    on lg.parent_loc_cd = l.location_cd
where l.location_type_cd = 123
