class ZCL_APC_WSP_EXT_ZAPC_TEST_PCP definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_PCP_B
  final
  create public .

public section.

  methods IF_APC_WSP_EXT_PCP~ON_START
    redefinition .
  methods IF_APC_WSP_EXT_PCP~ON_MESSAGE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZAPC_TEST_PCP IMPLEMENTATION.


  method IF_APC_WSP_EXT_PCP~ON_MESSAGE.
*CALL METHOD SUPER->IF_APC_WSP_EXT_PCP~ON_MESSAGE
*  EXPORTING
*    I_MESSAGE         =
*    I_MESSAGE_MANAGER =
*    I_CONTEXT         =
*    .

    DATA: lv_text TYPE string.

    DATA(lv_incoming_msg) =  i_message->get_field( 'TEST1' ).

    DATA(lo_message) = i_message_manager->create_message( ).

    lv_text = |Incoming Message: { lv_incoming_msg } | .
    lo_message->set_text( lv_text ).
    i_message_manager->send( lo_message ).
  endmethod.


  method IF_APC_WSP_EXT_PCP~ON_START.
*CALL METHOD SUPER->IF_APC_WSP_EXT_PCP~ON_START
*  EXPORTING
*    I_CONTEXT         =
*    I_MESSAGE_MANAGER =
*    .

    data(lo_message) = i_message_manager->create_message( ).
*    lo_message->set_text('Connected !').
    lo_message->set_field(
        i_name  = 'MESSAGE'
        i_value = 'Connected !'
    ).
*    CATCH cx_ac_message_type_pcp_error. " ABAP Channels message type Push Channel Protocol error
    i_message_manager->send( lo_message ).
  endmethod.
ENDCLASS.
