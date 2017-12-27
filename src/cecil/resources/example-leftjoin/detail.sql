select
  ocir.catalog_cd
from
  synonym_item_r sir
left join order_catalog_item_r ocir
  on ocir.item_id = sir.item_id
where sir.item_id not in (
  select
    1
  from
    dual
)
order by sir.item_id
