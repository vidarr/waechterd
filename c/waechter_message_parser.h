/**

        @author Michael J. Beer

*/
#ifndef WAECHTER_MESSAGE_PARSER_H
#define WAECHTER_MESSAGE_PARSER_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

#include "waechter_message.h"

/*----------------------------------------------------------------------------*/

typedef struct {

    W_MessageType type;

    uint32_t id;

    char const *body;

    char const *trailer;

} W_ParsedMessage;

extern W_ParsedMessage W_ParsedIncomplete;
extern W_ParsedMessage W_ParsedInvalid;

/*----------------------------------------------------------------------------*/

typedef struct {

    char *data;

} W_MessageOptions;

typedef struct {

    W_MessageOptions raw;

    char const *method;
    char const *user;
    char const *auth_info;

} W_Authenticate;

typedef struct {

    W_MessageOptions raw;

    char *raw_copy;
    char const *token;

} W_AuthenticateResponse;

typedef struct {

    W_MessageOptions raw;

    char const *token;
    char const *resource;

} W_Authorize;

/*----------------------------------------------------------------------------*/

W_ParsedMessage w_parse_message(char const *msg);

W_Authenticate w_parse_authenticate(W_Message msg);
W_AuthenticateResponse w_parse_authenticate_response(W_Message msg);

W_Authorize parse_authorize(W_Message msg);

bool w_encode_authenticate(char *target, size_t capacity, W_Authenticate opts);
bool w_encode_authorize(char *target, size_t capacity, W_Authorize opts);

bool w_encode_response(char *target, size_t capacity, bool response_state,
                       char const **opts);

bool w_encode_authenticate_response(char *target, size_t capacity,
                                    W_AuthenticateResponse opt);

/*----------------------------------------------------------------------------*/

void w_print_parsed_message(FILE *out, W_ParsedMessage msg);

/*----------------------------------------------------------------------------*/

#endif
