  select distinct
    sir.item_id,
    ocs.cat_id,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocir.cat_id AND ACTIVE_IND = 1 /*uar_get_code_display(ocir.cat_id)*/) AS ITEM_PRIMARY,
    sir.synonym_id,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocs.cat_id AND ACTIVE_IND = 1 /*uar_get_code_display(ocs.cat_id)*/) AS SYN_PRIMARY,
    ocs.mnemonic,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocs.mnemonic_type_id AND ACTIVE_IND = 1 /*uar_get_code_display(ocs.mnemonic_type_id)*/) AS MNEMONIC_TYPE_ID,
     mi.value AS ITEM_DESC
  from
    synonym_item_r sir
  inner join order_cat_item_r ocir
    on ocir.item_id = sir.item_id
  inner join order_cat_synonym ocs
    on ocs.synonym_id = sir.synonym_id
    and ocs.cat_id != ocir.cat_id
  left join med_identifier mi
    on mi.item_id = ocir.item_id
    and mi.med_product_id = 0
    and mi.primary_ind = 1
    and mi.med_identifier_type_id = (select CODE_VALUE from CODE_VALUE where cki = 'CKI.CODEVALUE!3290' and CODE_SET = 11000 and ACTIVE_IND = 1 )
  where sir.item_id not in (
    select
      ocir.item_id
    from
      order_cat_item_r ocir
    group by ocir.item_id
    having count(ocir.cat_id) > 1
  )
  order by sir.item_id
