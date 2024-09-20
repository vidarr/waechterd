/**

        @author         Michael J. Beer

*/

#include "waechter_message.h"

#include <stdlib.h>

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
