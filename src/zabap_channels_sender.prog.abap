*&---------------------------------------------------------------------*
*& Report ZABAP_CHANNEL_SENDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZABAP_CHANNELS_SENDER.

selection-screen begin of block a01 WITH FRAME title text-001.
  parameters: p_appid type IF_ABAP_CHANNEL_TYPES=>ty_amc_application_id,
              p_chnid type IF_ABAP_CHANNEL_TYPES=>ty_amc_channel_id,
              p_chexid type IF_ABAP_CHANNEL_TYPES=>ty_amc_channel_extension_id.
selection-screen end of block a01.

SELECTION-SCREEN begin of block a02 WITH FRAME title text-002.
  parameters: p_msg type string LOWER CASE.
selection-screen end of block a02.

START-OF-SELECTION.
  try.
    data(lo_dt_manager) = cl_amc_dt_manager=>create(
      i_application_id = p_appid
      i_channel_id = p_chnid
    ).

    data(lv_message_type) = lo_dt_manager->get_message_type( ).
    catch cx_amc_dt_error into data(amc_dt_error).
      message amc_dt_error->get_text( ) type 'I'.
  endtry.


  TRY.
    case lv_message_type.
      when 'PCP'.
        DATA(lo_message_prdcr_pcp) = CAST if_amc_message_producer_pcp(
          cl_amc_channel_manager=>create_message_producer(
            i_application_id = p_appid
            i_channel_id = p_chnid
            i_channel_extension_id = p_chexid
          )
        ).

        DATA(lo_pcp_message) = cl_ac_message_type_pcp=>create( ).
*        pcp_message->set_text( p_msg ).
        lo_pcp_message->set_field(
          i_name = 'MESSAGE'
          i_value = p_msg
        ).
        lo_message_prdcr_pcp->send( lo_pcp_message ).

      when 'TEXT'.
        data(lo_message_prdcr_text) = CAST if_amc_message_producer_text(
          cl_amc_channel_manager=>create_message_producer(
            i_application_id = p_appid
            i_channel_id = p_chnid
            i_channel_extension_id = p_chexid
          )
        ).

        lo_message_prdcr_text->send( p_msg ).

    endcase.
      message 'Message sent' type 'S'.
    catch cx_ac_message_type_pcp_error into data(pcp_error).
      message pcp_error->get_text( ) type 'I'.
    catch cx_amc_error into data(amc_error).
      message amc_error->get_text( ) type 'I'.
  ENDTRY.
