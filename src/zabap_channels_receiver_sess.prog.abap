*&---------------------------------------------------------------------*
*& Report ZABAP_CHANNELS_RECEIVER_SESS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZABAP_CHANNELS_RECEIVER_SESS.

data: gt_messages type table of string.

CLASS lcl_amc_receiver_pcp DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver_pcp.

ENDCLASS.

CLASS lcl_amc_receiver_pcp IMPLEMENTATION.

  METHOD if_amc_message_receiver_pcp~receive.

    DATA: lv_message    TYPE string,
          lt_pcp_fields TYPE pcp_fields.

    TRY.
*        lv_message = i_message->get_text( ).
        i_message->get_fields(
          CHANGING
            c_fields = lt_pcp_fields
        ).

        READ TABLE lt_pcp_fields INTO DATA(lw_pcp_field)
          WITH KEY name = 'MESSAGE'.
        IF sy-subrc EQ 0.
          append lw_pcp_field-value to gt_messages.
        ENDIF.

      CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
        lv_message = |RECEIVE ERROR: { pcp_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_amc_receiver_text DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver_text.

ENDCLASS.

CLASS lcl_amc_receiver_text IMPLEMENTATION.

  METHOD if_amc_message_receiver_text~receive.
    append i_message to gt_messages.
  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_appid  TYPE if_abap_channel_types=>ty_amc_application_id,
              p_chnid  TYPE if_abap_channel_types=>ty_amc_channel_id,
              p_chexid TYPE if_abap_channel_types=>ty_amc_channel_extension_id.
  parameters: p_msgcnt type i default 2,
              p_wait type i default 20.
SELECTION-SCREEN END OF BLOCK a01.

start-of-selection.
  TRY.
      DATA(lo_dt_manager) = cl_amc_dt_manager=>create(
        i_application_id = p_appid
        i_channel_id = p_chnid
      ).

      DATA(lv_message_type) = lo_dt_manager->get_message_type( ).
    CATCH cx_amc_dt_error INTO DATA(amc_dt_error).
      MESSAGE amc_dt_error->get_text( ) TYPE 'I'.
  ENDTRY.

  data(go_consumer) = cl_amc_channel_manager=>create_message_consumer(
    EXPORTING
      i_application_id       = p_appid
      i_channel_id           = p_chnid
      i_channel_extension_id = p_chexid ).

  CASE lv_message_type.
    WHEN 'PCP'.
      DATA(go_receiver_pcp) = NEW lcl_amc_receiver_pcp( ).
      go_consumer->start_message_delivery( go_receiver_pcp  ).

    WHEN 'TEXT'.
      DATA(go_receiver_text) = NEW lcl_amc_receiver_text( ).
      go_consumer->start_message_delivery( go_receiver_text  ).

  ENDCASE.

  WAIT FOR MESSAGING CHANNELS UNTIL
    lines( gt_messages ) >= p_msgcnt
    UP TO p_wait SECONDS.
  if sy-subrc eq 0.
    WRITE:/ 'Connected'.
    WRITE:/ 'Incoming messages will be listed below'.
    loop at gt_messages into data(lv_message).
      write:/ 'INCOMING MESSAGE: ', lv_message.
    endloop.
  else.
    message 'Timeout occured, no incoming messages' type 'I'.
  endif.
