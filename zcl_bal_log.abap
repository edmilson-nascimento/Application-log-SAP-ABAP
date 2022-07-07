
class zcl_application_log definition
  public
  final
  create public .

  public section.

    methods constructor
      importing title     type bal_s_log-extnumber
                object    type bal_s_log-object    optional
                subobject type bal_s_log-subobject optional
                alprog    type bal_s_log-alprog .

    methods add
      importing msg type bal_s_msg .

    methods add_bapiret2
      importing
        !is_msg       type bapiret2
      exporting
        !ev_lognumber type balognr .

    methods add_bapiret2_t
      importing
        !msg            type bapiret2_t
      returning
        value(rv_value) type balognr .

    methods save .

    methods show .

    class-methods show_saved
      importing
        !option type i default 0 .

    methods get_lognumber
      returning
        value(value) type balognr .

    class-methods data_out
      importing
        !i_data      type sy-datum
      returning
        value(value) type char10 .

    class-methods time_out
      importing
        !i_time      type sy-uzeit
      returning
        value(value) type char10 .

    class-methods save_all
      importing
        !it_messages    type bapiret2_t
      returning
        value(rv_value) type balognr .

  protected section.

  private section.

    class-data:
      gv_title     type bal_s_log-extnumber,
      gv_object    type bal_s_log-object,
      gv_subobject type bal_s_log-subobject,
      gv_alprog    type bal_s_log-alprog,
      gv_handles   type balloghndl,
      gv_lognumber type balognr.

    methods create
      changing handles type balloghndl .

    methods set_lognumber
      importing
        !i_lognumber type balognr .

endclass.


class zcl_application_log implementation.

  method constructor .

    gv_title     = title .
    gv_object    = object .
    gv_subobject = subobject .
    gv_alprog    = alprog .

    me->create(
      changing
        handles = gv_handles
    ).

  endmethod.

  method create.

    data:
      ls_log type bal_s_log .

    get time.

    ls_log-object     = gv_object .
    ls_log-subobject  = gv_subobject.
    ls_log-extnumber  = gv_title .
    ls_log-aldate     = sy-datum.
    ls_log-altime     = sy-uzeit.
    ls_log-aluser     = sy-uname.
*   ls_log-aldate_del = sy-datum + 30.
    ls_log-del_before = abap_true.
    ls_log-alprog     = gv_alprog .

*   Create_log.
    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log                 = ls_log
      importing
        e_log_handle            = handles
      exceptions
        log_header_inconsistent = 1
        others                  = 2.

    if sy-subrc ne 0 .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endmethod.


  method set_lognumber .

    if ( i_lognumber is not initial ) .
      me->gv_lognumber = i_lognumber .
    endif .

  endmethod .


  method add.

    if ( gv_handles is initial ) .
      me->create( changing handles = gv_handles ).
    endif .

    if ( gv_handles is not initial ) .

      call function 'BAL_LOG_MSG_ADD'
        exporting
          i_log_handle     = gv_handles
*         i_s_msg          = messages
          i_s_msg          = msg
        exceptions
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          others           = 4.

      if ( sy-subrc eq 0 ) .
*        IF ( gv_object IS NOT INITIAL ) .
*          me->save( ).
*        ENDIF .
      else .
        message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

  endmethod.


  method add_bapiret2 .

    data:
      msg_bal type bal_s_msg .

    clear ev_lognumber .

    if ( is_msg is not initial ) .

      msg_bal-msgty = is_msg-type .
      msg_bal-msgno = is_msg-number .
      msg_bal-msgid = is_msg-id .
      msg_bal-msgv1 = is_msg-message_v1 .
      msg_bal-msgv2 = is_msg-message_v2 .
      msg_bal-msgv3 = is_msg-message_v3 .
      msg_bal-msgv4 = is_msg-message_v4 .

      me->add( msg = msg_bal ) .

    endif .

  endmethod .


  method add_bapiret2_t .

    loop at msg into data(ls_message) .
      me->add_bapiret2( is_msg = ls_message ) .
    endloop .

    rv_value = me->get_lognumber( ) .

  endmethod .


  method save.

    data:
      lt_handles       type bal_t_logh,
      lv_save_all      type boolean,
      e_new_lognumbers type bal_t_lgnm.

    append gv_handles to lt_handles.

    call function 'BAL_DB_SAVE_PREPARE'
      exporting
        i_replace_in_all_logs = abap_on
*       i_t_replace_in_these_logs     =
*       i_t_replace_message_variables =
*       i_t_replace_context_fields    =
      exceptions
        log_not_found         = 1
        others                = 2.

    if ( sy-subrc ne 0 ) .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


    call function 'BAL_DB_SAVE'
      exporting
        i_client         = sy-mandt
*       i_in_update_task = space
        i_save_all       = lv_save_all
        i_t_log_handle   = lt_handles
*       i_2th_connection = space
*       i_2th_connect_commit = space
*       i_link2job       = 'x'
      importing
        e_new_lognumbers = e_new_lognumbers
*       e_second_connection  =
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.

    if ( sy-subrc eq 0 ) .

      if ( lines( e_new_lognumbers ) gt 0 ) .
        me->set_lognumber( e_new_lognumbers[ 1 ]-lognumber ) .
      endif .

    else .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    free lt_handles .

  endmethod.


  method show.

    data:
      lt_handles         type bal_t_logh,
      ls_display_profile type bal_s_prof.


*   get standard display profile
    call function 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      importing
        e_s_display_profile = ls_display_profile
      exceptions
        others              = 1.
    if ( sy-subrc ne 0 ) .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_s_display_profile  = ls_display_profile
        i_t_log_handle       = lt_handles
*       i_t_msg_handle       =
*       i_s_log_filter       =
*       i_s_msg_filter       =
*       i_t_log_context_filter        =
*       i_t_msg_context_filter        =
*       i_amodal             = space
*       i_srt_by_timstmp     = space
*       i_msg_context_filter_operator = 'a'
*        importing
*       e_s_exit_command     =
      exceptions
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        others               = 5.
    if sy-subrc ne 0 .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    free: lt_handles, ls_display_profile .

  endmethod.


  method show_saved.

*    DATA:
*      lt_handles         TYPE bal_t_logh,
*      ls_handles         TYPE balloghndl,
*      ls_display_profile TYPE bal_s_prof.
*
*    IF ( handle IS NOT INITIAL  ) .
*
*      CALL FUNCTION 'BAL_LOG_EXIST'
*        EXPORTING
*          i_log_handle  = handle
*        EXCEPTIONS
*          log_not_found = 1
*          OTHERS        = 2.
*
*      IF (  sy-subrc EQ 0 ) .
*
*        APPEND ls_handles TO lt_handles .
*        CLEAR  ls_handles .
*
*      ENDIF.
*
*    ENDIF .
*
*    IF ( lines( handles ) EQ 0  ) .
*    ELSE .
*
*      LOOP AT handles INTO ls_handles .
*
*        CALL FUNCTION 'BAL_LOG_EXIST'
*          EXPORTING
*            i_log_handle  = ls_handles
*          EXCEPTIONS
*            log_not_found = 1
*            OTHERS        = 2.
*
*        IF (  sy-subrc EQ 0 ) .
*
*          APPEND ls_handles TO lt_handles .
*          CLEAR  ls_handles .
*
*        ENDIF.
*
*      ENDLOOP.
*
**     get standard display profile
*      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
*        IMPORTING
*          e_s_display_profile = ls_display_profile
*        EXCEPTIONS
*          OTHERS              = 1.
*      IF ( sy-subrc NE 0 ) .
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*
*      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*        EXPORTING
*          i_s_display_profile  = ls_display_profile
*          i_t_log_handle       = lt_handles
**         i_t_msg_handle       =
**         i_s_log_filter       =
**         i_s_msg_filter       =
**         i_t_log_context_filter        =
**         i_t_msg_context_filter        =
**         i_amodal             = space
**         i_srt_by_timstmp     = space
**         i_msg_context_filter_operator = 'a'
**        importing
**         e_s_exit_command     =
*        EXCEPTIONS
*          profile_inconsistent = 1
*          internal_error       = 2
*          no_data_available    = 3
*          no_authority         = 4
*          OTHERS               = 5.
*
*      IF sy-subrc NE 0 .
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDIF.
*
*    FREE:
*      lt_handles, ls_display_profile .




    data:
      t_lognumber         type bal_t_logn,

      e_s_log_filter      type bal_s_lfil,
      l_t_log_header      type balhdr_t,
      l_t_log_handle      type bal_t_logh,
      i_log_handle        type balloghndl,
      read_from_db_hdr(1) type c,
      l_t_log_loaded      type bal_t_logh,
      l_t_locked          type balhdr_t,
      i_s_display_profile type  bal_s_prof,
      l_s_display_profile type bal_s_prof,
*      i_variant_report    TYPE  sy-repid VALUE 'SBAL_DISPLAY',
      number_of_protocols like  sy-dbcnt value 4,
      i_srt_by_timstmp    type  boolean
      .

    t_lognumber = value bal_t_logn( ( '00000000000001268738' ) ) .

    call function 'BAL_FILTER_CREATE'
      exporting
*       i_object       = object
*       i_subobject    = subobject
*       i_extnumber    = '00qwPT6L7jgsjQu6zl7SuW'
*       i_aldate_from  = sy-datum
*       i_aldate_to    = sy-datum
*       i_altime_from  = i_altime_from
*       i_altime_to    = i_altime_to
*       i_probclass_from = i_probclass_from
*       i_probclass_to = i_probclass_to
*       i_alprog       = i_alprog
*       i_altcode      = i_altcode
*       i_aluser       = i_aluser
*       i_almode       = i_almode
        i_t_lognumber  = t_lognumber
      importing
        e_s_log_filter = e_s_log_filter.

*if ( handle is initial ) .
*e_s_log_filter =
*  value #( log_handle ( handle ) ) ) .
*ENDIF .
*
*E_S_LOG_FILTER-LOG_HANDLE[1]-LOW

    call function 'BAL_DB_SEARCH'
      exporting
*       i_client           = SY-MANDT
        i_s_log_filter     = e_s_log_filter
*       i_t_sel_field      = i_t_sel_field
*       i_tzone            = i_tzone
      importing
        e_t_log_header     = l_t_log_header
      exceptions
        log_not_found      = 1
        no_filter_criteria = 2
        others             = 3.
    if sy-subrc <> 0.
    endif.

    clear l_t_log_handle.
    loop at l_t_log_header assigning field-symbol(<l_s_log_header>) .
      call function 'BAL_LOG_EXIST'
        exporting
          i_log_handle  = <l_s_log_header>-log_handle
        exceptions
          log_not_found = 1.
      if sy-subrc = 0.
        insert <l_s_log_header>-log_handle into table l_t_log_handle.
        delete l_t_log_header.
      endif.
    endloop.


    call function 'BAL_DB_LOAD'
      exporting
        i_t_log_header         = l_t_log_header
        i_do_not_load_messages = read_from_db_hdr
        i_lock_handling        = 1
      importing
        e_t_log_handle         = l_t_log_loaded
        e_t_locked             = l_t_locked
      exceptions
        others                 = 0.
    insert lines of l_t_log_loaded into table l_t_log_handle.

    describe table l_t_locked lines sy-tfill.
    if sy-tfill > 0.
      message s263(bl) with sy-tfill.
    endif.

    if not i_s_display_profile is initial.
      l_s_display_profile = i_s_display_profile.
    else.


*      IF number_of_protocols = 1.
*        CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
*          IMPORTING
*            e_s_display_profile = l_s_display_profile
*          EXCEPTIONS
*            OTHERS              = 0.
*      ELSE.
*        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
*          IMPORTING
*            e_s_display_profile = l_s_display_profile
*          EXCEPTIONS
*            OTHERS              = 0.
*      ENDIF.

      case option .
        when 0 .
          call function 'BAL_DSP_PROFILE_POPUP_GET'
            importing
              e_s_display_profile = l_s_display_profile
            exceptions
              others              = 0.
        when 1 .
          call function 'BAL_DSP_PROFILE_STANDARD_GET'
            importing
              e_s_display_profile = l_s_display_profile
            exceptions
              others              = 0.
        when 2 .
          call function 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
            importing
              e_s_display_profile = l_s_display_profile
            exceptions
              others              = 0.
        when 3 .
        when others .
      endcase .
    endif.


    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_t_log_handle      = l_t_log_handle
        i_s_display_profile = l_s_display_profile
        i_srt_by_timstmp    = i_srt_by_timstmp
      exceptions
        no_authority        = 1
        others              = 2.
    if sy-subrc <> 0.
    endif.

  endmethod.


  method get_lognumber .

    clear value .

    if ( me->gv_lognumber is not initial ) .
      value = me->gv_lognumber .
    endif .

  endmethod .


  method data_out .

    clear value .

    if ( i_data is not initial ) .

      call function 'CONVERSION_EXIT_PDATE_OUTPUT'
        exporting
          input  = i_data
        importing
          output = value.

    endif .

  endmethod .


  method time_out .

    clear value .

    if ( i_time is not initial ) .

      write i_time to value using edit mask '__:__:__' .

    endif .

  endmethod .


  method save_all .


    data:
      lv_title     type balnrext  value 'Log de Kanban',
      lv_object    type balobj_d  value 'ZPP',
      lv_subobject type balsubobj value 'ZPP0001'.

    if ( lv_object    is not initial ) and
       ( lv_subobject is not initial ) .

      data(lo_app) = new zcl_application_log( title     = lv_title
                                              object    = lv_object
                                              subobject = lv_subobject
                                              alprog    = sy-cprog ) .

      if ( lo_app is bound ) .

        rv_value = lo_app->add_bapiret2_t( it_messages ) .

        lo_app->save( ) .

        free lo_app .

      endif .

    endif .

  endmethod .


endclass.
