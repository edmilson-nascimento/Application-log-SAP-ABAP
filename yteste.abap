*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yteste.


CONSTANTS:
  title     TYPE balnrext  VALUE 'Test Development',
  object    TYPE balobj_d  VALUE 'ZFI',
  subobject TYPE balsubobj VALUE 'ZFI_P_S_AC'.


DATA:
  app_log        TYPE REF TO zcl_fi_application_log,
  msg_log        TYPE bal_s_msg,
  saved_message  TYPE balloghndl,
  saved_messages TYPE bapiret2_t.

IF ( object    IS NOT INITIAL ) AND
   ( subobject IS NOT INITIAL ) .

*  CREATE OBJECT app_log
*    EXPORTING
*      title     = title
*      object    = object
*      subobject = subobject
*      alprog    = sy-cprog.
*
*
*  IF app_log IS BOUND .
*
**   Mensagem de exemplo
*    msg_log-msgty    = 'I' .
*    msg_log-msgno    = 000 .
*    msg_log-msgid    = '>0' .
*    msg_log-msgv1    = 'Info' .
*    msg_log-msgv2    = sy-datum  .
*    msg_log-msgv3    = sy-uzeit  .
*    msg_log-msgv4    = '' .
*
**    app_log->add( msg_log ).
**
***   Mensagem de exemplo
**    msg_log-msgty    = 'I' .
**    msg_log-msgno    = 000 .
**    msg_log-msgid    = '>0' .
**    msg_log-msgv1    = 'Test Saved Messages' .
**    msg_log-msgv2    = sy-datum  .
**    msg_log-msgv3    = sy-uzeit  .
**    msg_log-msgv4    = '' .
**
**    app_log->add( msg_log ).
*
*    msg_log-msgty    = '' .
*    msg_log-msgno    = 000 .
*    msg_log-msgid    = '>0' .
*    msg_log-msgv1    = 'Warning' .
*    msg_log-msgv2    = sy-datum  .
*    msg_log-msgv3    = sy-uzeit  .
*    msg_log-msgv4    = app_log->get_handles( ) .
*    saved_message    = app_log->get_handles( ) .
*
*    app_log->add( msg_log ).
*
*    app_log->save( ) .
*
*    FREE app_log .
*
*  ENDIF .

*  IF ( app_log IS NOT BOUND ) .
*
*    CREATE OBJECT app_log
*      EXPORTING
*        title     = title
*        object    = object
*        subobject = subobject
*        alprog    = sy-cprog.
*
*
*    IF ( app_log IS BOUND ) .
*
*      DATA(handles) =
*        VALUE bal_t_logh(
*          ( '00qwPT6L7jgsbK6bt0hMBm' )
*          ( '00qwPT6L7jgsbM7ETNgMDW' )
*        ) .
*
*      app_log->show_saved( handles = handles ).
*
*    ENDIF .
*
*  ENDIF .


ENDIF .

FREE app_log .



DATA:
  e_s_log_filter      TYPE bal_s_lfil,
  l_t_log_header      TYPE balhdr_t,
  l_t_log_handle      TYPE bal_t_logh,
  i_log_handle        TYPE balloghndl,
  read_from_db_hdr(1) TYPE c,
  l_t_log_loaded      TYPE bal_t_logh,
  l_t_locked          TYPE balhdr_t,
  i_s_display_profile TYPE  bal_s_prof,
  l_s_display_profile TYPE bal_s_prof,
  i_variant_report    TYPE  sy-repid VALUE 'SBAL_DISPLAY',
  number_of_protocols LIKE  sy-dbcnt VALUE 4,
  i_srt_by_timstmp    TYPE  boolean
  .

CALL FUNCTION 'BAL_FILTER_CREATE'
  EXPORTING
    i_object       = 'ZFI'
*   i_subobject    = i_subobject
*   i_extnumber    = i_extnumber
    i_aldate_from  = sy-datum
    i_aldate_to    = sy-datum
*   i_altime_from  = i_altime_from
*   i_altime_to    = i_altime_to
*   i_probclass_from = i_probclass_from
*   i_probclass_to = i_probclass_to
*   i_alprog       = i_alprog
*   i_altcode      = i_altcode
*   i_aluser       = i_aluser
*   i_almode       = i_almode
*   i_t_lognumber  = i_t_lognumber
  IMPORTING
    e_s_log_filter = e_s_log_filter.



CALL FUNCTION 'BAL_DB_SEARCH'
  EXPORTING
*   i_client           = SY-MANDT
    i_s_log_filter     = e_s_log_filter
*   i_t_sel_field      = i_t_sel_field
*   i_tzone            = i_tzone
  IMPORTING
    e_t_log_header     = l_t_log_header
  EXCEPTIONS
    log_not_found      = 1
    no_filter_criteria = 2
    OTHERS             = 3.
IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
  IF number_of_protocols = 1.
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = l_s_display_profile
      EXCEPTIONS
        OTHERS              = 0.
  ELSE.
    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = l_s_display_profile
      EXCEPTIONS
        OTHERS              = 0.
  ENDIF.
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
  CASE sy-subrc.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING no_authority.
    WHEN 2.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.
ENDIF.
