class zcl_pp_conf_status_kanban definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_afpo,
        aufnr type afpo-aufnr,
        posnr type afpo-posnr,
        matnr type afpo-matnr,
      end of ty_afpo .
    types:
      tab_afpo    type table of ty_afpo .
    types:
      tab_out     type table of zpps0046 .
    types:
      tab_matnr   type range of mseg-matnr .
    types:
      tab_orderid type range of zppt0004-orderid .
    types:
      tab_budat   type range of mkpf-budat .

    data gt_outtab type tab_out .

    methods constructor .
    methods cenario_ok
      importing
        !iv_matnr       type tab_matnr optional
        !iv_werks       type t001w-werks
        !iv_aufnr       type caufvd-aufnr
        !iv_menge       type caufvd-bmenge
        !iv_meins       type caufvd-bmeins
        !iv_prvbe       type prvbe optional
        !iv_mblnr       type mblnr
        !iv_mjahr       type mjahr
        !iv_bwart       type mseg-bwart
      returning
        value(rv_value) type abap_bool .
    methods atualiza_status
      importing
        !is_data   type zppt0004
      exporting
        !et_return type bapiret2_t .
    methods get_data
      importing
        !iv_refresh type abap_bool default ''
        !iv_werks   type mseg-werks optional
        !ir_matnr   type zcl_pp_conf_status_kanban=>tab_matnr optional
        !ir_orderid type zcl_pp_conf_status_kanban=>tab_orderid optional
        !ir_budat   type zcl_pp_conf_status_kanban=>tab_budat optional .
    methods process_data .
    methods show_information .
    methods dados_rel_ok
      returning
        value(rv_value) type abap_bool .
    methods get_zppt0004
      returning
        value(rt_value) type zppc0028 .
    methods get_value_c
      importing
        !iv_value       type menge_d
      returning
        value(rv_value) type char18 .

  protected section.

    methods on_added_function
      for event if_salv_events_functions~added_function
          of cl_salv_events_table
      importing
          !e_salv_function .

    methods on_link_click
      for event if_salv_events_actions_table~link_click
                  of cl_salv_events_table
      importing row
                  column .

  private section.

    data go_salv_table type ref to cl_salv_table .
    data gv_werks type mseg-werks .
    data gr_matnr type zcl_pp_conf_status_kanban=>tab_matnr .
    data gr_orderid type zcl_pp_conf_status_kanban=>tab_orderid .
    data gr_budat type zcl_pp_conf_status_kanban=>tab_budat .
    data:
      gt_zppt0004   type standard table of zppt0004 .
    data:
      gt_makt       type sorted table of makt with unique default key .
    data gt_afpo type tab_afpo .

    methods get_guid
      returning
        value(rv_return) type char20 .
    methods salva_log
      importing
        !it_message     type bapiret2_t
      returning
        value(rv_value) type zppt0004-lognumber .
    methods atualiza_tabela_z
      importing
        !is_data type zppt0004 .
    methods refresh .
    methods reprocessar
      importing
        !is_data        type zppt0004
      returning
        value(rv_value) type abap_bool .
    methods get_material_ordem
      importing
        !iv_aufnr       type caufvd-aufnr
      returning
        value(rv_value) type tab_matnr .
    methods tp_mov_ok
      importing
        !iv_bwart       type mseg-bwart
      returning
        value(rv_value) type abap_bool .
endclass.



class zcl_pp_conf_status_kanban implementation.


  method atualiza_status .

    data:
      lt_return  type bapiret2_t,
      ls_return  type bapiret2,
      lv_message type t100-text.

    if ( is_data-menge is not initial ) .

      try .

          call function 'BAPI_KANBANCC_WITHDRAWQUANTITY'
            exporting
              kanbancontrolcycle = is_data-pknum
              withdr_qty         = value bapi1172_withdr_qty( withdr_qty = is_data-menge )
              physicaltime       = value bapi1075_phystime( physdate = sy-datum
                                                            phystime = sy-uzeit )
*             kanbanbarcode      =
*             kanbanid           =
*             kanbanno           =
*             exclude_status_restriction = SPACE
            importing
              return             = ls_return
*           tables
*             statuschangeresult =
            .

          if ( ls_return is not initial ) .
            append ls_return to lt_return .
          endif .

        catch cx_sy_dyn_call_illegal_type .

          lt_return =
            value #( ( type       = if_xo_const_message=>success
                       id         = '>0'
                       number     = 000
                       message_v1 = 'Exceção'
                       message_v2 = 'ao executar function'
                       message_v3 = 'BAPI_KANBANCC_WITHDRAWQUANTITY' ) ) .

      endtry .

      if ( line_exists( lt_return[ type = if_xo_const_message=>error ] ) ) .

      else .

        if ( lines( lt_return ) eq 0 ) .
          lt_return =
            value #( ( type       = if_xo_const_message=>success
                       id         = '>0'
                       number     = 000
                       message_v1 = 'Status do Kanban'
                       message_v2 = 'atualizado com sucesso.' ) ) .
        endif .

      endif .

    endif .

    " Preparando o log e a tabela de atualizacao
    data(ls_data) = is_data .

    " Informando chave unica
    if ( ls_data-guid is initial ) .
      ls_data-guid = me->get_guid( ) .
    endif .

    if ( lines( lt_return ) gt 0 ) .
      ls_data-lognumber = me->salva_log( lt_return ) .
    endif .

    " Verificando flag de reprocessamento
    if ( line_exists( lt_return[ type = if_xo_const_message=>error ] ) ) .
      ls_data-zreproc = abap_on .
    else .
      ls_data-zreproc = abap_off .
    endif .

    loop at lt_return assigning field-symbol(<fs_message>) .

      clear lv_message .

      call function 'FORMAT_MESSAGE'
        exporting
          id        = <fs_message>-id
*         lang      = '-D'
          no        = <fs_message>-number
          v1        = <fs_message>-message_v1
          v2        = <fs_message>-message_v2
          v3        = <fs_message>-message_v3
          v4        = <fs_message>-message_v4
        importing
          msg       = lv_message
        exceptions
          not_found = 1
          others    = 2.

      if ( sy-subrc eq 0 ) and
         ( lv_message is not initial ) .

        if ( ls_data-zlog is initial ) .
          ls_data-zlog = lv_message .
        else .
          ls_data-zlog = |{ lv_message } / { ls_data-zlog }| .
        endif .

      endif .

    endloop .

    me->atualiza_tabela_z( ls_data ) .


  endmethod .


  method atualiza_tabela_z .

    if ( is_data is not initial ) .
      modify zppt0004 from is_data .
    endif .

  endmethod .


  method cenario_ok .

    data:
      lv_sfcpf type marc-sfcpf value is initial,
      lt_mseg  type table of imseg.

    clear rv_value .

*    call function 'MB_ANALYZE_IMSEG'
*      tables
*        ct_imseg = lt_mseg.

    if ( iv_werks is not initial ) and
       ( me->tp_mov_ok( iv_bwart ) eq abap_true ) and
       ( ( sy-ucomm eq 'OK_POST' ) or ( sy-ucomm is initial )
                                   or ( sy-ucomm eq 'OK_POST1' ) )  .

      if ( lines( iv_matnr ) gt 0 ) .

        data(lr_matnr) = iv_matnr .
        sort lr_matnr ascending by low .
        delete adjacent duplicates from lr_matnr comparing low .

      else .
        lr_matnr = me->get_material_ordem( iv_aufnr ) .
      endif .

      " Verificar quais os componentes da estrutura IMSEG tem Kanban ativo
      select matnr, werks, sfcpf
        from marc
        into table @data(lt_marc)
       where matnr in @lr_matnr
         and werks eq @iv_werks .

      if ( sy-subrc eq 0 ) .

        delete lt_marc where sfcpf eq lv_sfcpf .

        if ( lines( lt_marc ) gt 0 ) .

          select pknum, matnr, werks, behaz
            from pkhd
            into table @data(lt_pkhd)
             for all entries in @lt_marc
           where matnr eq @lt_marc-matnr
             and werks eq @lt_marc-werks .

          if ( sy-subrc eq 0 ) .

            data(lr_pkbst) =
              value bapi1075_pkbst_range_tab( let s = rsmds_c_sign-including
                                                  o = rsmds_c_option-equal
                                               in sign   = s
                                                  option = o
                                              ( status_low = '2' )
                                              ( status_low = '3' )
                                              ( status_low = '5' )
                                              ( status_low = '6' ) ) .
            select pkkey, pknum
              from pkps
              into table @data(lt_pkps)
               for all entries in @lt_pkhd
             where pknum eq @lt_pkhd-pknum
               and pkbst in @lr_pkbst .

            if ( sy-subrc eq 0 ) .

              rv_value = abap_on .

              data(lv_pknum) = value #( lt_pkps[ 1 ]-pknum optional ) .

              me->gt_zppt0004 =
                value #(  (
                           pknum     = lv_pknum
                           behaz     = value #( lt_pkhd[ pknum = lv_pknum ]-behaz optional )
                           orderid   = iv_aufnr
                           idnrk     = value #( lt_pkhd[ pknum = lv_pknum ]-matnr optional )
                           werks     = iv_werks
                           menge     = iv_menge
                           meins     = iv_meins
                           prvbe     = iv_prvbe

                           mblnr     = iv_mblnr
                           mjahr     = iv_mjahr

                           budat     = sy-datum
                           cputm     = sy-uzeit
                           usnam     = sy-uname ) ) .

            endif .

          endif .

        endif .

      endif .

    endif .

  endmethod .


  method constructor .

    clear:
      gv_werks, gr_matnr, gr_orderid, gr_budat, gt_makt .

  endmethod .


  method dados_rel_ok .

    if ( lines( me->gt_outtab ) gt 0 ) .
      rv_value = abap_on .
    else .
      rv_value = abap_off .
    endif .

  endmethod .


  method get_data .

    refresh:
      me->gt_zppt0004, me->gt_outtab, me->gt_makt .

    " Verificando os campos internos para atribuicao
    if ( iv_refresh eq abap_false ) .
      me->gv_werks   = iv_werks .
      me->gr_matnr   = ir_matnr .
      me->gr_orderid = ir_orderid .
      me->gr_budat   = ir_budat .
    endif .

    " Este filtro deveria ser obrigatorio
    data(lt_where) =
      value usp_t_selection_conditions( ( line = 'werks eq me->gv_werks' ) ) .

    if ( lines( me->gr_matnr ) gt 0 ) .
      append 'and matnr in me->gr_matnr' to lt_where .
    endif .

    if ( lines( me->gr_orderid ) gt 0 ) .
      append 'and orderid in me->gr_orderid ' to lt_where .
    endif .

    if ( lines( me->gr_budat ) gt 0 ) .
      append 'and budat in me->gr_budat  ' to lt_where .
    endif .

    if ( lines( lt_where ) eq 0 ) .
    else .

      try .
          select *
            into table me->gt_zppt0004
            from zppt0004
           where (lt_where) .
        catch cx_sy_dynamic_osql_semantics .
      endtry .

      if ( sy-subrc eq 0 ) .

        data(lr_matnr) =
          value zcl_pp_conf_status_kanban=>tab_matnr(
            for l in me->gt_zppt0004 ( sign   = rsmds_c_sign-including
                                       option = rsmds_c_option-equal
                                       low    = l-idnrk ) ) .

        data(lr_aufnr) =
          value range_t_aufnr(
            for l in me->gt_zppt0004 ( sign   = rsmds_c_sign-including
                                       option = rsmds_c_option-equal
                                       low    = l-orderid ) ) .

        sort lr_aufnr ascending by low .
        delete adjacent duplicates from lr_aufnr comparing low .
        delete lr_aufnr where low is initial .

        if ( lines( lr_aufnr ) gt 0 ) .

          select aufnr posnr matnr
            from afpo
            into table me->gt_afpo
           where aufnr in lr_aufnr .

          if ( sy-subrc eq 0 ) .

            data(lr_matnr_tmp) =
              value zcl_pp_conf_status_kanban=>tab_matnr(
                for a in me->gt_afpo ( sign   = rsmds_c_sign-including
                                       option = rsmds_c_option-equal
                                       low    = a-matnr ) ) .

            append lines of lr_matnr_tmp to lr_matnr .

          endif .

        endif .

        sort lr_matnr ascending by low .
        delete adjacent duplicates from lr_matnr comparing low .
        delete lr_matnr where low is initial .

        if ( lines( lr_matnr ) gt 0 ) .

          select *
           from makt
           into table me->gt_makt
          where matnr in lr_matnr
            and spras eq sy-langu .

        endif .

      endif .

    endif .

  endmethod .


  method get_guid.

    data:
      lv_string type string,
      lv_number type i value 20.

    clear rv_return .

    call function 'GENERAL_GET_RANDOM_STRING'
      exporting
        number_chars  = lv_number
      importing
        random_string = lv_string.

    rv_return = lv_string .

  endmethod.


  method get_material_ordem .

    clear rv_value .

    if ( iv_aufnr is not initial ) .

      select aufnr, rsnum, plnbez
        from afko
        into table @data(lt_afko)
       where aufnr eq @iv_aufnr .

      if ( sy-subrc eq 0 ) .

        select rsnum, rspos, rsart, matnr, bwart
          from resb
          into table @data(lt_resb)
           for all entries in @lt_afko
         where rsnum eq @lt_afko-rsnum .

        if ( sy-subrc eq 0 ) .

          rv_value = value #( for r in lt_resb ( sign   = rsmds_c_sign-including
                                                 option = rsmds_c_option-equal
                                                 low    = r-matnr ) ) .
        endif .

      endif .

    endif .

  endmethod .


  method get_value_c .

    data:
      lv_number type p length 7 decimals 3.

    clear rv_value .

    if ( iv_value is not initial ) .

      " convert the float to char to make it readable
      call function 'CEVA_CONVERT_FLOAT_TO_CHAR'
        exporting
          float_imp  = iv_value
          format_imp = lv_number
*         LEFT_IMP   = ' '
*         ROUND_IMP  = ' '
        importing
          char_exp   = rv_value
*         OVERFLOW_EXP =
        .
      condense rv_value .

    endif.

  endmethod .


  method get_zppt0004 .

    rt_value = me->gt_zppt0004 .

  endmethod .


  method on_link_click .


    read table me->gt_outtab assigning field-symbol(<line>) index row .
    if ( sy-subrc eq 0 ) .

      assign component 'TRKORR' of structure <line> to field-symbol(<field>).
      if <field> is assigned .

        case column .

          when 'TRKORR' .

*                call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
*                  exporting
*                    iv_trkorr = trkorr
**                   iv_dirtype             =
**                   iv_without_check       = ' '
            .

          when 'OBJECTS' .

**                call function 'TRINT_TDR_USER_COMMAND'
**                  exporting
**                    iv_object  = trkorr
**                    iv_type    = 'TASK'
**                    iv_command = 'REQUEST_SHOW'
***                  importing
***                   ev_exit    =
*                .
*                call function 'TRINT_TDR_USER_COMMAND'
*                  exporting
*                    iv_object  = trkorr
*                    iv_type    = 'REQU'
*                    iv_command = 'REQUEST_SHOW'
**                  importing
**                   ev_exit    =
*                  .

          when others .

        endcase .

      endif.

    endif.


  endmethod .


  method on_added_function .


    case e_salv_function .

      when 'REFRESH' .

        me->refresh( ) .

      when 'RUN' .

        data(lt_rows)   = go_salv_table->get_selections( )->get_selected_rows( ).
        data(lv_index)  = value #( lt_rows[ 1 ] optional ).
        data(ls_outtab) = value #( me->gt_outtab[ lv_index ] optional ) .

        if ( ls_outtab is not initial ) .
          do 3 times .
            zcl_application_log=>show_saved( iv_lognumber = ls_outtab-lognumber
                                             option       = sy-index ) .
          enddo .
        endif .


      when 'REPROCESS' .

        lt_rows   = go_salv_table->get_selections( )->get_selected_rows( ).
        lv_index  = value #( lt_rows[ 1 ] optional ).
        ls_outtab = value #( me->gt_outtab[ lv_index ] optional ) .

        if ( ls_outtab is not initial ) .

          if ( ls_outtab-zreproc eq abap_true ) . " Permitir reprocessamento?

            data(ls_zppt0004) = value #( me->gt_zppt0004[ guid    = ls_outtab-guid
                                                          pknum   = ls_outtab-pknum
                                                          behaz   = ls_outtab-behaz
                                                          orderid = ls_outtab-orderid ] optional ) .

            if ( me->reprocessar( ls_zppt0004 ) eq abap_true ) .
              call function 'BAPI_TRANSACTION_COMMIT' .
            endif .

          else .

            message w000(>0) with 'Não permitido processamento deste item.' .

          endif .

        endif .

        me->refresh( ) .

      when others .

    endcase .

  endmethod .


  method process_data .

    me->gt_outtab =
      value #( for l in me->gt_zppt0004 (
        guid       = l-guid
        pknum      = l-pknum
        orderid    = l-orderid
        behaz      = l-behaz
       "matnr      = value #( me->gt_afpo[ aufnr = l-pknum ]-matnr optional )
        matnr      = value #( me->gt_afpo[ aufnr = l-orderid ]-matnr optional )
       "matnr_desc = value #( me->gt_makt[ matnr = value #( me->gt_afpo[ aufnr = l-pknum ]-matnr
        matnr_desc = value #( me->gt_makt[ matnr = value #( me->gt_afpo[ aufnr = l-orderid ]-matnr
                                                            optional ) ]-maktx optional )
        idnrk      = l-idnrk
        idnrk_desc = value #( me->gt_makt[ matnr = l-idnrk ]-maktx optional )
        werks      = l-werks
        menge      = l-menge
        meins      = l-meins
        budat      = l-budat
        cputm      = l-cputm
        usnam      = l-usnam
        mblnr      = l-mblnr
        mjahr      = l-mjahr
        zlog       = l-zlog
        zreproc    = l-zreproc
        zdtarep    = l-zdtarep
        zusnam2    = l-zusnam2
        lognumber  = l-lognumber ) ) .

  endmethod .


  method refresh .

    me->get_data( iv_refresh = abap_on ) .
    me->process_data( ) .

    me->go_salv_table->refresh( ) .

  endmethod .


  method reprocessar .

    rv_value = abap_off .

    if ( is_data is not initial ) .

      me->atualiza_status( is_data ) .
      rv_value = abap_on .

    endif .

  endmethod .


  method salva_log .

    clear rv_value .

    if ( lines( it_message ) gt 0 ) .
      rv_value = zcl_application_log=>save_all( it_message ) .
    endif .

  endmethod .


  method show_information .

    data:
      lo_events     type ref to cl_salv_events_table,
      lo_columns    type ref to cl_salv_columns_table,
      lo_column     type ref to cl_salv_column,
      lo_selections type ref to cl_salv_selections,
      lo_display    type ref to cl_salv_display_settings.


    if ( lines( me->gt_outtab ) eq 0 ) .
    else .

      try .

          cl_salv_table=>factory(
*             exporting
*             list_display = if_salv_c_bool_sap=>true
            importing
              r_salv_table = go_salv_table
            changing
              t_table      = me->gt_outtab ) .

          " Eventos do relatório
          lo_events = go_salv_table->get_event( ).
          set handler me->on_added_function for lo_events.
          set handler me->on_link_click for lo_events.

          " Habilita opção de selecionar linha
          lo_selections = go_salv_table->get_selections( ).
          if ( lo_selections is bound ) .
            lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
          endif .

          " Otimizar largura da columa
          lo_columns = go_salv_table->get_columns( ) .
          if ( lo_columns is bound ) .
            lo_columns->set_optimize( cl_salv_display_settings=>true ).

            lo_column = lo_columns->get_column( columnname = 'GUID' ) .
            if ( lo_column is bound ) .
              lo_column->set_visible( if_salv_c_bool_sap=>false ) .
              free lo_column.
            endif .

            lo_column = lo_columns->get_column( columnname = 'MBLNR' ) .
            if ( lo_column is bound ) .
              lo_column->set_visible( if_salv_c_bool_sap=>false ) .
              free lo_column.
            endif .

          endif .

          " Usando status standard
          go_salv_table->set_screen_status(
            pfstatus      = 'STANDARD_FULLSCREEN'
            report        = 'ZPPR_0023'
            set_functions = go_salv_table->c_functions_all ) .

          " Layout de Zebra
          lo_display = go_salv_table->get_display_settings( ) .
          if ( lo_display is bound ) .
            lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .
          endif .

          lo_display = go_salv_table->get_display_settings( ) .
          lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .

          go_salv_table->display( ).

        catch cx_salv_msg .
        catch cx_salv_not_found .
        catch cx_salv_existing .
        catch cx_salv_data_error .
        catch cx_salv_object_not_found .

      endtry.

    endif .

  endmethod .


  method tp_mov_ok .

    data:
      lv_name type tvarvc-name value 'ZPP_TP_MOV_KANBAN' .

    rv_value = abap_off .

    if ( iv_bwart is not initial ) .

      select name, type, numb, sign, opti, low, high
        from tvarvc
        into table @data(lt_data)
       where name eq @lv_name .

      if ( sy-subrc eq 0 ) .

        if ( line_exists( lt_data[ low = iv_bwart ] ) ) .
          rv_value = abap_on .
        endif .

      endif .

    endif .

  endmethod .


endclass.
