
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

    METHODS save .

    METHODS show .

    METHODS get_handles
      RETURNING
        VALUE(value) TYPE balloghndl .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA:
      gv_title     TYPE bal_s_log-extnumber,
      gv_object    TYPE bal_s_log-object,
      gv_subobject TYPE bal_s_log-subobject,
      gv_alprog    TYPE bal_s_log-alprog,
      gv_handles   TYPE balloghndl.

    METHODS create
      CHANGING handles TYPE balloghndl .

ENDCLASS.


CLASS zcl_fi_application_log IMPLEMENTATION.

  METHOD constructor .

    gv_title     = title .
    gv_object    = object .
    gv_subobject = subobject .
    gv_alprog    = alprog .

  ENDMETHOD.

  METHOD create.

    DATA:
      ls_log TYPE bal_s_log .

    ls_log-extnumber = gv_title.
    ls_log-object    = gv_object .
    ls_log-subobject = gv_subobject .
    ls_log-alprog    = gv_alprog .

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


  METHOD save.

    DATA:
      lt_handles TYPE bal_t_logh .

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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
*       i_in_update_task = space
        i_save_all       = abap_on
        i_t_log_handle   = lt_handles
*       i_2th_connection = space
*       i_2th_connect_commit = space
*       i_link2job       = 'x'
*      importing
*       e_new_lognumbers =
*       e_second_connection  =
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF ( sy-subrc NE 0 ) .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    FREE: lt_handles .

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


  METHOD get_handles .

    CLEAR value .

    IF ( me->gv_handles IS NOT INITIAL ) .
      value = me->gv_handles .
    ENDIF .

  ENDMETHOD .


ENDCLASS.
