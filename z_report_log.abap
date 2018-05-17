report z_report_log .

type-pools: abap .

class lcl_class definition .

  public section .

    class-methods date
      importing
        !input type sydatum
      returning
        value(output) type symsgv .

    class-methods time
      importing
        !input type syuzeit
      returning
        value(output) type symsgv .

  protected section .

  private section .

    class-methods convert
      importing
        !name   type rs38l-name
        !input  type any
      returning value(output) type symsgv .


endclass .

class lcl_class implementation .

  method date .

    if input is not initial .

      output =
        lcl_class=>convert(
          name   = 'CONVERSION_EXIT_PDATE_OUTPUT'
          input = input
        ).

    endif .

  endmethod .

  method time .

    if input is not initial .

      output =
        lcl_class=>convert(
          name   = 'CONVERSION_EXIT_HH_MM_OUTPUT'
          input = input
        ).

    endif .

  endmethod .

  method convert .

    call function name
      exporting
        input         = input
      importing
        output        = output .

  endmethod .

endclass .

data:
  msg_log type        bal_s_msg,
  app_log type ref to zcl_bal_log.


start-of-selection .

  create object app_log
    exporting
      title     = 'Titulo do Log'
*     object    =
*     subobject =
      alprog    = sy-cprog.


  if app_log is bound .

*   Mensagem de exemplo

    msg_log-msgty    = 'I' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Informação' .
    msg_log-msgv2    = lcl_class=>date( sy-datum ) .
    msg_log-msgv3    = lcl_class=>time( sy-uzeit ) .
*   msg_log-msgv4    = '' .
    app_log->add( msg_log ).

    clear msg_log .
    msg_log-msgty    = 'E' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Erro' .
    msg_log-msgv2    = lcl_class=>date( sy-datum ) .
    msg_log-msgv3    = lcl_class=>time( sy-uzeit ) .
*   msg_log-msgv4    = '' .
    app_log->add( msg_log ).


    clear msg_log .
    msg_log-msgty    = 'W' .
    msg_log-msgno    = 000 .
    msg_log-msgid    = '>0' .
    msg_log-msgv1    = 'Atenção' .
    msg_log-msgv2    = lcl_class=>date( sy-datum ) .
    msg_log-msgv3    = lcl_class=>time( sy-uzeit ) .
*   msg_log-msgv4    = '' .
    app_log->add( msg_log ).

  endif .


end-of-selection .

if app_log is bound .

  if app_log->exists( ) eq 0 .

    app_log->show( ) .

  endif .

