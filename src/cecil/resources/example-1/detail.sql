select distinct
  sir.item_id,
  (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocir.catalog_cd AND ACTIVE_IND = 1 /*uar_get_code_display(ocir.catalog_cd)*/) AS ITEM_PRIMARY
from
  synonym_item_r sir
inner join order_catalog_item_r ocir
  on ocir.item_id = sir.item_id
where sir.item_id not in (
  select
    1
  from
    dual
)
order by sir.item_id
