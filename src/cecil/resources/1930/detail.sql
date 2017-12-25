  select distinct
    sir.item_id,
    ocs.catalog_cd,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocir.catalog_cd AND ACTIVE_IND = 1 /*uar_get_code_display(ocir.catalog_cd)*/) AS ITEM_PRIMARY,
    sir.synonym_id,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocs.catalog_cd AND ACTIVE_IND = 1 /*uar_get_code_display(ocs.catalog_cd)*/) AS SYN_PRIMARY,
    ocs.mnemonic,
    (SELECT DISPLAY FROM CODE_VALUE WHERE CODE_VALUE = ocs.mnemonic_type_cd AND ACTIVE_IND = 1 /*uar_get_code_display(ocs.mnemonic_type_cd)*/) AS MNEMONIC_TYPE_CD,
     mi.value AS ITEM_DESC
  from
    synonym_item_r sir
  inner join order_catalog_item_r ocir
    on ocir.item_id = sir.item_id
  inner join order_catalog_synonym ocs
    on ocs.synonym_id = sir.synonym_id
    and ocs.catalog_cd != ocir.catalog_cd
  left join med_identifier mi
    on mi.item_id = outerjoin(ocir.item_id)
    and mi.med_product_id = outerjoin(0)
    and mi.primary_ind = outerjoin(1)
    and mi.med_identifier_type_cd = outerjoin(value((select CODE_VALUE from CODE_VALUE cv where cv.cki = 'CKI.CODEVALUE!3290' and ACTIVE_IND = 1 )))
  where sir.item_id not in (
    select
      ocir.item_id
    from
      order_catalog_item_r ocir
    group by ocir.item_id
    having count(ocir.catalog_cd) > 1
  )
  order by sir.item_id
