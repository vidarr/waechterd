/**

        @author Michael J. Beer

*/
#ifndef WAECHTER_MESSAGE_H
#define WAECHTER_MESSAGE_H

#include <stdint.h>
#include <stdio.h>

/*----------------------------------------------------------------------------*/

typedef enum {

    W_INVALID = -1,
    W_INCOMPLETE,
    W_REQUEST_AUTHENTICATE,
    W_REQUEST_AUTHORIZE,
    W_RESPONSE_OK,
    W_RESPONSE_ERROR

} W_MessageType;

typedef struct {

    W_MessageType type;

    uint32_t id;

    char *body;

} W_Message;

extern W_Message W_Incomplete;
extern W_Message W_Invalid;

/*----------------------------------------------------------------------------*/

void w_message_clear(W_Message *msg);

/*----------------------------------------------------------------------------*/

void w_print_message(FILE *out, W_Message msg);

/*----------------------------------------------------------------------------*/

#endif
