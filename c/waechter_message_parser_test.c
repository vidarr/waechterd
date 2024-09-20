/**

        @author         Michael J. Beer

*/

#include "waechter_message_parser.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "test_helpers.c"

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

int main() { test_w_parse_message(); }
