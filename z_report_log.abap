
report report_log .

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
  ls_msg    type        bal_s_msg,
  o_bal_log type ref to zcl_bal_log.


start-of-selection .

  create object o_bal_log
    exporting
      title  = 'Titulo do Log'
*     object =
*     subobject =
      alprog = sy-cprog.


  if o_bal_log is bound .

*   Mensagem de exemplo

    ls_msg-msgty    = 'I' .
    ls_msg-msgno    = 000 .
    ls_msg-msgid    = '>0' .
    ls_msg-msgv1    = 'Informação' .
    ls_msg-msgv2    = lcl_class=>date( sy-datum ) .
    ls_msg-msgv3    = lcl_class=>time( sy-uzeit ) .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).

    clear ls_msg .
    ls_msg-msgty    = 'E' .
    ls_msg-msgno    = 000 .
    ls_msg-msgid    = '>0' .
    ls_msg-msgv1    = 'Erro' .
    ls_msg-msgv2    = lcl_class=>date( sy-datum ) .
    ls_msg-msgv3    = lcl_class=>time( sy-uzeit ) .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).


    clear ls_msg .
    ls_msg-msgty    = 'W' .
    ls_msg-msgno    = 000 .
    ls_msg-msgid    = '>0' .
    ls_msg-msgv1    = 'Atenção' .
    ls_msg-msgv2    = lcl_class=>date( sy-datum ) .
    ls_msg-msgv3    = lcl_class=>time( sy-uzeit ) .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).

  endif .


end-of-selection .

  if o_bal_log is bound .
    o_bal_log->show( ) .
  endif .
