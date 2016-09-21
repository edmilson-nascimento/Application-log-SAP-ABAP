
report report_log .


type-pools: abap .

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
*   ls_msg-msgv2    = '' .
*   ls_msg-msgv3    = '' .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).

    clear ls_msg .
    ls_msg-msgty    = 'E' .
    ls_msg-msgno    = 000 .
    ls_msg-msgid    = '>0' .
    ls_msg-msgv1    = 'Erro' .
*   ls_msg-msgv2    = '' .
*   ls_msg-msgv3    = '' .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).


    clear ls_msg .
    ls_msg-msgty    = 'W' .
    ls_msg-msgno    = 000 .
    ls_msg-msgid    = '>0' .
    ls_msg-msgv1    = 'Atenção' .
*   ls_msg-msgv2    = '' .
*   ls_msg-msgv3    = '' .
*   ls_msg-msgv4    = '' .
    o_bal_log->add( msg = ls_msg ).

  endif .


end-of-selection .

  if o_bal_log is bound .
    o_bal_log->show( ) .
  endif .