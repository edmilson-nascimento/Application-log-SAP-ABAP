
CLASS zcl_fi_application_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING title     TYPE bal_s_log-extnumber
                object    TYPE bal_s_log-object    OPTIONAL
                subobject TYPE bal_s_log-subobject OPTIONAL
                alprog    TYPE bal_s_log-alprog .

    METHODS add
      IMPORTING msg TYPE bal_s_msg .

    METHODS add_bapiret2
      IMPORTING
        !msg       TYPE bapiret2
      EXPORTING
        !lognumber TYPE balognr .

    METHODS add_bapiret2_t
      IMPORTING
        !msg TYPE bapiret2_t .

    METHODS save .

    METHODS show .

    CLASS-METHODS show_saved
      IMPORTING
        !option TYPE i DEFAULT 0 .

    METHODS get_lognumber
      RETURNING
        VALUE(value) TYPE balognr .

    CLASS-METHODS data_out
      IMPORTING
        !i_data      TYPE sy-datum
      RETURNING
        VALUE(value) TYPE char10 .

    CLASS-METHODS time_out
      IMPORTING
        !i_time      TYPE sy-uzeit
      RETURNING
        VALUE(value) TYPE char10 .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA:
      gv_title     TYPE bal_s_log-extnumber,
      gv_object    TYPE bal_s_log-object,
      gv_subobject TYPE bal_s_log-subobject,
      gv_alprog    TYPE bal_s_log-alprog,
      gv_handles   TYPE balloghndl,
      gv_lognumber TYPE balognr.

    METHODS create
      CHANGING handles TYPE balloghndl .

    METHODS set_lognumber
      IMPORTING
        !i_lognumber TYPE balognr .

ENDCLASS.


CLASS zcl_fi_application_log IMPLEMENTATION.

  METHOD constructor .

    gv_title     = title .
    gv_object    = object .
    gv_subobject = subobject .
    gv_alprog    = alprog .

    me->create(
      CHANGING
        handles = gv_handles
    ).

  ENDMETHOD.

  METHOD create.

    DATA:
      ls_log TYPE bal_s_log .

    GET TIME.

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
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = handles
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD set_lognumber .

    IF ( i_lognumber IS NOT INITIAL ) .
      me->gv_lognumber = i_lognumber .
    ENDIF .

  ENDMETHOD .


  METHOD add.

    IF ( gv_handles IS INITIAL ) .
      me->create( CHANGING handles = gv_handles ).
    ENDIF .

    IF ( gv_handles IS NOT INITIAL ) .

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = gv_handles
*         i_s_msg          = messages
          i_s_msg          = msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      IF ( sy-subrc EQ 0 ) .
*        IF ( gv_object IS NOT INITIAL ) .
*          me->save( ).
*        ENDIF .
      ELSE .
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD add_bapiret2 .

    DATA:
      msg_bal TYPE bal_s_msg .

    CLEAR lognumber .

    IF ( msg IS NOT INITIAL ) .

      msg_bal-msgty = msg-type .
      msg_bal-msgno = msg-number .
      msg_bal-msgid = msg-id .
      msg_bal-msgv1 = msg-message_v1 .
      msg_bal-msgv2 = msg-message_v2 .
      msg_bal-msgv3 = msg-message_v3 .
      msg_bal-msgv4 = msg-message_v4 .

      me->add( msg = msg_bal ) .
      lognumber = me->get_lognumber( ) .
      me->save( ).

    ENDIF .

  ENDMETHOD .


  METHOD add_bapiret2_t .
  ENDMETHOD .


  METHOD save.

    DATA:
      lt_handles       TYPE bal_t_logh,
      lv_save_all      TYPE boolean,
      e_new_lognumbers TYPE bal_t_lgnm.

    APPEND gv_handles TO lt_handles.

    CALL FUNCTION 'BAL_DB_SAVE_PREPARE'
      EXPORTING
        i_replace_in_all_logs = abap_on
*       i_t_replace_in_these_logs     =
*       i_t_replace_message_variables =
*       i_t_replace_context_fields    =
      EXCEPTIONS
        log_not_found         = 1
        OTHERS                = 2.

    IF ( sy-subrc NE 0 ) .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
*       i_in_update_task = space
        i_save_all       = lv_save_all
        i_t_log_handle   = lt_handles
*       i_2th_connection = space
*       i_2th_connect_commit = space
*       i_link2job       = 'x'
      IMPORTING
        e_new_lognumbers = e_new_lognumbers
*       e_second_connection  =
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF ( sy-subrc EQ 0 ) .

      IF ( lines( e_new_lognumbers ) GT 0 ) .
        me->set_lognumber( e_new_lognumbers[ 1 ]-lognumber ) .
      ENDIF .

    ELSE .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    FREE lt_handles .

  ENDMETHOD.


  METHOD show.

    DATA:
      lt_handles         TYPE bal_t_logh,
      ls_display_profile TYPE bal_s_prof.


*   get standard display profile
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF ( sy-subrc NE 0 ) .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
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
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    FREE: lt_handles, ls_display_profile .

  ENDMETHOD.


  METHOD show_saved.

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




    DATA:
      t_lognumber         TYPE bal_t_logn,

      e_s_log_filter      TYPE bal_s_lfil,
      l_t_log_header      TYPE balhdr_t,
      l_t_log_handle      TYPE bal_t_logh,
      i_log_handle        TYPE balloghndl,
      read_from_db_hdr(1) TYPE c,
      l_t_log_loaded      TYPE bal_t_logh,
      l_t_locked          TYPE balhdr_t,
      i_s_display_profile TYPE  bal_s_prof,
      l_s_display_profile TYPE bal_s_prof,
*      i_variant_report    TYPE  sy-repid VALUE 'SBAL_DISPLAY',
      number_of_protocols LIKE  sy-dbcnt VALUE 4,
      i_srt_by_timstmp    TYPE  boolean
      .

    t_lognumber = VALUE bal_t_logn( ( '00000000000001268738' ) ) .

    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
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
      IMPORTING
        e_s_log_filter = e_s_log_filter.

*if ( handle is initial ) .
*e_s_log_filter =
*  value #( log_handle ( handle ) ) ) .
*ENDIF .
*
*E_S_LOG_FILTER-LOG_HANDLE[1]-LOW

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
*       i_client           = SY-MANDT
        i_s_log_filter     = e_s_log_filter
*       i_t_sel_field      = i_t_sel_field
*       i_tzone            = i_tzone
      IMPORTING
        e_t_log_header     = l_t_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ENDIF.

    CLEAR l_t_log_handle.
    LOOP AT l_t_log_header ASSIGNING FIELD-SYMBOL(<l_s_log_header>) .
      CALL FUNCTION 'BAL_LOG_EXIST'
        EXPORTING
          i_log_handle  = <l_s_log_header>-log_handle
        EXCEPTIONS
          log_not_found = 1.
      IF sy-subrc = 0.
        INSERT <l_s_log_header>-log_handle INTO TABLE l_t_log_handle.
        DELETE l_t_log_header.
      ENDIF.
    ENDLOOP.


    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header         = l_t_log_header
        i_do_not_load_messages = read_from_db_hdr
        i_lock_handling        = 1
      IMPORTING
        e_t_log_handle         = l_t_log_loaded
        e_t_locked             = l_t_locked
      EXCEPTIONS
        OTHERS                 = 0.
    INSERT LINES OF l_t_log_loaded INTO TABLE l_t_log_handle.

    DESCRIBE TABLE l_t_locked LINES sy-tfill.
    IF sy-tfill > 0.
      MESSAGE s263(bl) WITH sy-tfill.
    ENDIF.

    IF NOT i_s_display_profile IS INITIAL.
      l_s_display_profile = i_s_display_profile.
    ELSE.


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

      CASE option .
        WHEN 0 .
          CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 1 .
          CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 2 .
          CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
            IMPORTING
              e_s_display_profile = l_s_display_profile
            EXCEPTIONS
              OTHERS              = 0.
        WHEN 3 .
        WHEN OTHERS .
      ENDCASE .
    ENDIF.


    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle      = l_t_log_handle
        i_s_display_profile = l_s_display_profile
        i_srt_by_timstmp    = i_srt_by_timstmp
      EXCEPTIONS
        no_authority        = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD get_lognumber .

    CLEAR value .

    IF ( me->gv_lognumber IS NOT INITIAL ) .
      value = me->gv_lognumber .
    ENDIF .

  ENDMETHOD .


  METHOD data_out .

    CLEAR value .

    IF ( i_data IS NOT INITIAL ) .

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = i_data
        IMPORTING
          output = value.

    ENDIF .

  ENDMETHOD .


  METHOD time_out .

    CLEAR value .

    IF ( i_time IS NOT INITIAL ) .

      WRITE i_time TO value USING EDIT MASK '__:__:__' .

    ENDIF .

  ENDMETHOD .


ENDCLASS.
