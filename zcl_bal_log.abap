class ZCL_BAL_LOG definition
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
    methods save .
    methods show .

  protected section.
  private section.

    class-data:
      gv_title     type bal_s_log-extnumber,
      gv_object    type bal_s_log-object,
      gv_subobject type bal_s_log-subobject,
      gv_alprog    type bal_s_log-alprog,
      gv_handles   type balloghndl.

    methods create
      changing  handles type balloghndl .

endclass.


class zcl_bal_log implementation.

  method constructor .

    gv_title     = title .
    gv_object    = object .
    gv_subobject = subobject .
    gv_alprog    = alprog .

  endmethod.

  method create.

    data:
      ls_log type bal_s_log .

    ls_log-extnumber = gv_title.
    ls_log-object    = gv_object .
    ls_log-subobject = gv_subobject .
    ls_log-alprog    = gv_alprog .

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

  method add.

*   Cria o Log caso ainda não tenha sido feito
    if gv_handles is initial .
      me->create( changing handles = gv_handles ).
    endif .

    if gv_handles is not initial .

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

      if sy-subrc eq 0 .
        if gv_object is not initial .
          me->save( ).
        endif .
      else .
        message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

  endmethod.


  method save.

    data:
      lt_handles type bal_t_logh .

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
    if sy-subrc ne 0 .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    call function 'BAL_DB_SAVE'
      exporting
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
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc ne 0 .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    free: lt_handles .

  endmethod.

  method show.

    data:
      lt_handles         type bal_t_logh,
      ls_display_profile type bal_s_prof .


*   get standard display profile
    call function 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      importing
        e_s_display_profile = ls_display_profile
      exceptions
        others              = 1.
    if sy-subrc ne 0 .
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
  
  method EXISTS.

    data:
      handles type balmsghndl .

    if ( me->gv_handles is not initial ) .

      handles-log_handle = me->gv_handles .
      handles-msgnumber  = '000001' .

      call function 'BAL_LOG_MSG_READ'
        exporting
          i_s_msg_handle                 = handles
  *       i_langu                        = sy-langu
  *     importing
  *       e_s_msg                        =
  *       e_exists_on_db                 =
  *       e_txt_msgty                    =
  *       e_txt_msgid                    =
  *       e_txt_detlevel                 =
  *       e_txt_probclass                =
  *       e_txt_msg                      =
  *       e_warning_text_not_found       =
        exceptions
          log_not_found                  = 1
          msg_not_found                  = 2
          others                         = 3 .

      value = sy-subrc .

    else .

      value = 9 .

    endif .

  endmethod.  

endclass.
