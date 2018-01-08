select
  sir.item_id
from
  synonym_item_r sir
where sir.item_id is null
  or sir.item_id is not null