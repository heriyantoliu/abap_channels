class ZCL_APC_WSP_EXT_ZAPC_TEST definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZAPC_TEST IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_message.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_MESSAGE
*  EXPORTING
*    I_MESSAGE         =
*    I_MESSAGE_MANAGER =
*    I_CONTEXT         =
*    .

    DATA: lv_text TYPE string.

    DATA(lv_incoming_msg) =  i_message->get_text( ).

    DATA(lo_message) = i_message_manager->create_message( ).

    lv_text = |Incoming Message: { lv_incoming_msg } | .
    lo_message->set_text( lv_text ).
    i_message_manager->send( lo_message ).

    TRY.
        data(lo_message_prdcr_text) = CAST if_amc_message_producer_text(
          cl_amc_channel_manager=>create_message_producer(
            i_application_id = 'ZAMC_TEST'
            i_channel_id = '/APCTEXT'
            i_channel_extension_id = 'EXT1'
          )
        ).

        lo_message_prdcr_text->send( lv_incoming_msg ).

      CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
        MESSAGE pcp_error->get_text( ) TYPE 'E'.
      CATCH cx_amc_error INTO DATA(amc_error).
        MESSAGE amc_error->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  method IF_APC_WSP_EXTENSION~ON_START.
*CALL METHOD SUPER->IF_APC_WSP_EXTENSION~ON_START
*  EXPORTING
*    I_CONTEXT         =
*    I_MESSAGE_MANAGER =
*    .

    data(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text('Connected !').
    i_message_manager->send( lo_message ).

    data(binding) = i_context->get_binding_manager( ).
    binding->bind_amc_message_consumer(
      i_application_id = 'ZAMC_TEST'
      i_channel_id = '/apctext'
      i_channel_extension_id = 'EXT1'
    ).

    binding->bind_amc_message_consumer(
      i_application_id = 'ZAMC_TEST'
      i_channel_id = '/apcpcp'
      i_channel_extension_id = 'EXT1'
    ).

  endmethod.
ENDCLASS.
