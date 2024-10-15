/**

        @author         Michael J. Beer

*/

#include "waechter_message_parser.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "test_helpers.c"
#include <stdlib.h>

/*----------------------------------------------------------------------------*/

static bool message_parses_to(char const *strmsg, W_MessageType type,
                              uint32_t id, char const *trailer) {
    W_ParsedMessage msg = w_parse_message(strmsg);

    fprintf(stderr, "\n\nMessage string: %s\n", strmsg);
    w_print_parsed_message(stderr, msg);

    return (msg.type == type) && (msg.id == id) &&
           strequal(trailer, msg.trailer);

}

/*----------------------------------------------------------------------------*/

static void test_w_parse_message() {
    assert(message_parses_to(0, W_INVALID, 0, 0));
    assert(message_parses_to("\n   authorize  ab12  3.13.zerbst \n", W_INVALID,
                             0, 0));

    assert(message_parses_to("", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("  ", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("1321", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("1321   ", W_INCOMPLETE, 0, 0));

    assert(message_parses_to("1321 authen", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("1321 authenticate", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("1321 authenticate ", W_INCOMPLETE, 0, 0));
    assert(
        message_parses_to("1321 authenticate heinz plai", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("1321 authenticate heinz plai\n",
                             W_REQUEST_AUTHENTICATE, 1321, ""));
    assert(message_parses_to("1321 authenticate heinz plai\n pw",
                             W_REQUEST_AUTHENTICATE, 1321, " pw"));

    assert(message_parses_to("143 autho", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("143 authorize", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("143 authorize ", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("143 authorize heinz plai", W_INCOMPLETE, 0, 0));
    assert(message_parses_to("143 authorize heinz plai\n", W_REQUEST_AUTHORIZE,
                             143, ""));
    assert(message_parses_to("143 authorize heinz plai\n ", W_REQUEST_AUTHORIZE,
                             143, " "));
    assert(message_parses_to("143 authorize heinz plai\n ORC",
                             W_REQUEST_AUTHORIZE, 143, " ORC"));

    assert(message_parses_to("31634 r:", W_INCOMPLETE, 0, 0));

    assert(message_parses_to("31634 r:ok\n", W_RESPONSE_OK, 31634, ""));
    assert(message_parses_to("31635 r:ok \n", W_RESPONSE_OK, 31635, ""));
    assert(message_parses_to("31634 r:ok\nTrailer", W_RESPONSE_OK, 31634,
                             "Trailer"));
    assert(message_parses_to("31635 r:ok \n    Octopus", W_RESPONSE_OK, 31635,
                             "    Octopus"));
    assert(message_parses_to("31654 r:ok adsf 3324nadsf1234 \n", W_RESPONSE_OK,
                             31654, ""));
    assert(message_parses_to("31654 r:ok adsf 3324nadsf1234 \n ana",
                             W_RESPONSE_OK, 31654, " ana"));

    assert(message_parses_to("31634 r:error\n", W_RESPONSE_ERROR, 31634, ""));
    assert(message_parses_to("31635 r:error \n", W_RESPONSE_ERROR, 31635, ""));
    assert(message_parses_to("31634 r:error\nTrailer", W_RESPONSE_ERROR, 31634,
                             "Trailer"));
    assert(message_parses_to("31635 r:error \n    Octopus", W_RESPONSE_ERROR,
                             31635, "    Octopus"));
    assert(message_parses_to("31654 r:error adsf 3324nadsf1234 \n",
                             W_RESPONSE_ERROR, 31654, ""));
    assert(message_parses_to(
        "1132 r:error 2349234sdafsdt234534asrgtsgsdfagsdfgsdefrasdf\n aX",
        W_RESPONSE_ERROR, 1132, " aX"));

    assert(message_parses_to("31634 r:fail\n", W_INVALID, 0, 0));
}

/*----------------------------------------------------------------------------*/

static bool message_body_is(W_MessageBody body,
                            W_MessageType type,
                            char const *s1,
                            char const *s2,
                            char const *s3) {

    fprintf(stderr, "\n\nExpected: %s\n", w_message_type_to_string(type));

    if(body.type != type) {
        return false;
    } else {
    switch (body.type) {

        case W_INVALID:
        case W_INCOMPLETE:
        case W_RESPONSE_OK:
        case W_RESPONSE_ERROR:
            return true;

        case W_REQUEST_AUTHENTICATE:

            return strequal(body.authenticate.user, s1) &&
            strequal(body.authenticate.method, s2) &&
            strequal(body.authenticate.auth_info, s3);

        case W_REQUEST_AUTHORIZE:

            return strequal(body.authorize.resource, s1) &&
            strequal(body.authorize.token, s2);

        case W_RESPONSE_AUTHENTICATE_OK:

            return strequal(body.authenticate_response.token, s1);

    };

}

}

/*----------------------------------------------------------------------------*/

static W_Message make_message(W_MessageType type, char const *body) {

    return (W_Message) {

        .type = type,
        .id = rand(),
        .body = (char *)body,
    };

}

/*----------------------------------------------------------------------------*/

static void test_w_parse_authenticate() {

    W_MessageBody w_parse_authenticate(W_Message msg);
    assert(message_body_is(
        w_parse_authenticate(make_message(W_INVALID, 0)), W_INVALID, 0, 0, 0));

}

/*----------------------------------------------------------------------------*/

W_MessageBody w_parse_authenticate_response(W_Message msg);
W_MessageBody parse_authorize(W_Message msg);

bool w_encode_authenticate(char *target, size_t capacity, W_Authenticate opts);
bool w_encode_authorize(char *target, size_t capacity, W_Authorize opts);

int main() {

    test_w_parse_message();
    test_w_parse_authenticate();
}

/*----------------------------------------------------------------------------*/
