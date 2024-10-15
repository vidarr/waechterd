/**

        @author         Michael J. Beer

*/

#include "waechter_message.h"

#include <stdlib.h>

/*----------------------------------------------------------------------------*/

char const *w_message_type_to_string(W_MessageType type) {

    switch(type) {

    case W_INVALID:
        return "INVALID";

    case W_INCOMPLETE:
        return "INCOMPLETE";

    case W_REQUEST_AUTHENTICATE:
        return "AUTHENTICATE";

    case W_REQUEST_AUTHORIZE:
        return "REQUEST_AUTHORIZE";

    case W_RESPONSE_OK:
        return "RESPONSE_OK";

    case W_RESPONSE_ERROR:
        return "RESPONSE_ERROR";

    case W_RESPONSE_AUTHENTICATE_OK:
        return "RESPONSE_AUTHENTICATE_OK";

    };

}

/*----------------------------------------------------------------------------*/

W_Message W_Incomplete = {

    .type = W_INCOMPLETE,

};

W_Message W_Invalid = {

    .type = W_INVALID,

};

/*----------------------------------------------------------------------------*/

void w_message_clear(W_Message *msg) {

    if(0 != msg) {

        free((char *)msg->body);
        msg->body = 0;

    }

}

/*----------------------------------------------------------------------------*/
