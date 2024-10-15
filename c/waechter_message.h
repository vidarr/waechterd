/**

        @author Michael J. Beer

*/
#ifndef WAECHTER_MESSAGE_H
#define WAECHTER_MESSAGE_H

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

/*----------------------------------------------------------------------------*/

typedef enum {

    W_INVALID = -1,
    W_INCOMPLETE,
    W_REQUEST_AUTHENTICATE,
    W_REQUEST_AUTHORIZE,
    W_RESPONSE_OK,
    W_RESPONSE_ERROR,
    W_RESPONSE_AUTHENTICATE_OK,

} W_MessageType;

char const *w_message_type_to_string(W_MessageType type);

/*----------------------------------------------------------------------------*/

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

/*****************************************************************************
                                 Messag bodies
 ****************************************************************************/

typedef struct {

    char const *method;
    char const *user;
    char const *auth_info;

} W_Authenticate;

typedef struct {

    char const *raw_copy;
    char const *token;

} W_AuthenticateResponse;

typedef struct {

    char const *token;
    char const *resource;

} W_Authorize;

typedef struct {

    W_MessageType type;

    union {

        W_Authenticate authenticate;
        W_AuthenticateResponse authenticate_response;
        W_Authorize authorize;
    };

    char *body_copy;

} W_MessageBody;

/*----------------------------------------------------------------------------*/

W_MessageBody w_message_body_parse(W_Message msg);

bool w_message_body_clear(W_MessageBody *msg_body);

/*----------------------------------------------------------------------------*/
#endif
