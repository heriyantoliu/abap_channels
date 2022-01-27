*&---------------------------------------------------------------------*
*& Report ZABAP_CHANNELS_RECEIVER_GUI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_channels_receiver_gui.

CLASS lcl_amc_receiver_pcp DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver_pcp.

ENDCLASS.

CLASS lcl_amc_receiver_pcp IMPLEMENTATION.

  METHOD if_amc_message_receiver_pcp~receive.

    DATA: lv_message    TYPE string,
          lt_pcp_fields TYPE pcp_fields.

    TRY.
        lv_message = i_message->get_text( ).
        i_message->get_fields(
          CHANGING
            c_fields = lt_pcp_fields
        ).

        READ TABLE lt_pcp_fields INTO DATA(lw_pcp_field)
          WITH KEY name = 'MESSAGE'.
        IF sy-subrc EQ 0.
          WRITE:/ 'INCOMING MESSAGE: ', lw_pcp_field-value.
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
    WRITE:/ 'INCOMING MESSAGE: ', i_message.
  ENDMETHOD.
ENDCLASS.

DATA: go_consumer TYPE REF TO if_amc_message_consumer.

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_appid  TYPE if_abap_channel_types=>ty_amc_application_id,
              p_chnid  TYPE if_abap_channel_types=>ty_amc_channel_id LOWER CASE,
              p_chexid TYPE if_abap_channel_types=>ty_amc_channel_extension_id LOWER CASE.
SELECTION-SCREEN END OF BLOCK a01.

START-OF-SELECTION.

  TRY.
      DATA(lo_dt_manager) = cl_amc_dt_manager=>create(
        i_application_id = p_appid
        i_channel_id = p_chnid
      ).

      DATA(lv_message_type) = lo_dt_manager->get_message_type( ).
    CATCH cx_amc_dt_error INTO DATA(amc_dt_error).
      MESSAGE amc_dt_error->get_text( ) TYPE 'I'.
  ENDTRY.

  DATA(dynamic_class) = `\PROGRAM=CL_AMC_CHANNEL_MANAGER========CP` && `\CLASS=LCL_SAPGUI_CHANNEL_MANAGER`.

  CALL METHOD (dynamic_class)=>create_message_consumer
    EXPORTING
      i_application_id       = p_appid
      i_channel_id           = p_chnid
      i_channel_extension_id = p_chexid
    RECEIVING
      r_consumer             = go_consumer.

  try.
    CASE lv_message_type.
      WHEN 'PCP'.
        DATA(go_receiver_pcp) = NEW lcl_amc_receiver_pcp( ).
        go_consumer->start_message_delivery( go_receiver_pcp  ).

      WHEN 'TEXT'.
        DATA(go_receiver_text) = NEW lcl_amc_receiver_text( ).
        go_consumer->start_message_delivery( go_receiver_text  ).

    ENDCASE.
    catch cx_root.
      message 'Error' type 'I'.
  endtry.

  WRITE:/ 'Connected'.
  WRITE:/ 'Incoming messages will be listed below'.
