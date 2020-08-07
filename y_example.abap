*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yteste.


CONSTANTS:
  title     TYPE balnrext  VALUE 'Test Development',
  object    TYPE balobj_d  VALUE 'ZFI',
  subobject TYPE balsubobj VALUE 'ZIF_P_S_AC'.

DATA:
  app_log        TYPE REF TO zcl_fi_application_log,
  msg_log        TYPE bal_s_msg,
  saved_message  TYPE balloghndl,
  saved_messages TYPE bapiret2_t.

IF ( object    IS NOT INITIAL ) AND
   ( subobject IS NOT INITIAL ) .

  CREATE OBJECT app_log
    EXPORTING
      title     = title
      object    = object
      subobject = subobject
      alprog    = sy-cprog.


  IF app_log IS BOUND .

*   Mensagem de exemplo
    msg_log-msgty    = 'I' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Info' .
    msg_log-msgv2    = sy-datum  .
    msg_log-msgv3    = sy-uzeit  .
    msg_log-msgv4    = '' .

    app_log->add( msg_log ).

*   Mensagem de exemplo
    msg_log-msgty    = 'I' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Test Saved Messages' .
    msg_log-msgv2    = sy-datum  .
    msg_log-msgv3    = sy-uzeit  .
    msg_log-msgv4    = '' .

    app_log->add( msg_log ).

    msg_log-msgty    = 'W' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Warning' .
    msg_log-msgv2    = sy-datum  .
    msg_log-msgv3    = sy-uzeit  .
    msg_log-msgv4    = app_log->get_handles( ) .
    saved_message    = app_log->get_handles( ) .

    app_log->add( msg_log ).

    app_log->save( ) .

    FREE app_log .

  ENDIF .

  IF ( app_log IS NOT BOUND ) .

    CREATE OBJECT app_log
      EXPORTING
        title     = title
        object    = object
        subobject = subobject
        alprog    = sy-cprog.


    IF ( app_log IS BOUND ) .

      DATA(handles) =
        VALUE bal_t_logh( ( saved_message ) ) .

      app_log->show_saved( handles = handles ).

    ENDIF .

  ENDIF .


ENDIF .


FREE app_log .
