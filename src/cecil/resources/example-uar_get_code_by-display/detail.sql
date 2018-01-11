select *
from
  med_identifier mi
where mi.med_identifier_type_id = (select CODE_VALUE from CODE_VALUE where DISPLAY = 'Described by' and CODE_SET = 11000 and ACTIVE_IND = 1 )
