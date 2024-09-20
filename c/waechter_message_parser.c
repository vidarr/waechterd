/**

        @author         Michael J. Beer

*/

#include "waechter_message_parser.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdlib.h>

/*----------------------------------------------------------------------------*/

W_ParsedMessage W_ParsedIncomplete = {

    .type = W_INCOMPLETE,

};

W_ParsedMessage W_ParsedInvalid = {

    .type = W_INVALID,

};

/*----------------------------------------------------------------------------*/

static char const *strsan(char const *s1) { return (0 == s1) ? "" : s1; }

/*----------------------------------------------------------------------------*/

static char const *skip_to_end_of_word(char const *str) {
    for (; (*str != 0) && (*str != ' '); ++str)
        ;
    return str;
}

/*----------------------------------------------------------------------------*/

static char const *skip_to_word(char const *str) {
    if (0 != str) {
        for (; (*str != 0) && (*str == ' '); ++str)
            ;
    }
    return str;
}

/*----------------------------------------------------------------------------*/

static bool eol_found(char const *str, char const **eol) {
    if (0 != str) {
        for (; (*str != 0) && (*str != '\n'); ++str)
            ;

        if (0 != eol) {
            *eol = str;
        }

        return *str == '\n';

    } else {
        return false;
    }
}

/*----------------------------------------------------------------------------*/

typedef struct {
    W_MessageType type;
    char const *endptr;

} ParsedMessageType;

ParsedMessageType invalid = {

    .type = W_INVALID,

};

ParsedMessageType incomplete = {

    .type = W_INCOMPLETE,

};

static ParsedMessageType parse_response(char const *str) {

    ParsedMessageType ok = {
        .type = W_RESPONSE_OK,
    };

    ParsedMessageType error = {
        .type = W_RESPONSE_ERROR,
    };

    if (0 == str) {
        return invalid;

    } else if (str[0] == 0) {
        return incomplete;

    } else if (str[0] != ':') {
        return invalid;
    }

    char c = toupper(str[1]);

    if (c == 0) {
        return incomplete;
    }

    if (c == 'O') {
        if (str[2] == 0) {
            return incomplete;
        } else if (toupper(str[2]) != 'K') {
            return invalid;
        } else if (str[3] == '\n') {
            ok.endptr = str + 3;
            return ok;
        } else if (str[3] == ' ') {
            if (!eol_found(str + 3, 0)) {
                return incomplete;
            } else {
                ok.endptr = skip_to_word(str + 3);
                return ok;
            }
        } else {
            return invalid;
        }
    } else if (c == 'E') {
        if ((0 == str[2]) || (0 == str[3]) || (0 == str[4]) || (0 == str[5]) ||
            (0 == str[6])) {
            return incomplete;

        } else if (('R' != toupper(str[2])) || ('R' != toupper(str[3])) ||
                   ('O' != toupper(str[4])) || ('R' != toupper(str[5]))) {
            return invalid;

        } else if (str[6] == '\n') {
            error.endptr = str + 6;
            return error;
        } else if (str[6] == ' ') {
            if (!eol_found(str + 6, 0)) {
                return incomplete;
            } else {
                error.endptr = skip_to_word(str + 6);
                return error;
            }
        } else {
            return invalid;
        }
    } else {
        return invalid;
    }
}

/*----------------------------------------------------------------------------*/

static ParsedMessageType parse_authenticate_type(char const *str) {
    ParsedMessageType authenticate = {
        .type = W_REQUEST_AUTHENTICATE,
    };

    if (0 == str) {
        return invalid;
        //      NTI CAT E
    } else if ((0 == str[0]) || (0 == str[1]) || (0 == str[2]) ||
               (0 == str[3]) || (0 == str[4]) || (0 == str[5]) ||
               (0 == str[6]) || (0 == str[7])) {
        return incomplete;
    } else if (('N' != toupper(str[0])) || ('T' != toupper(str[1])) ||
               ('I' != toupper(str[2])) || ('C' != toupper(str[3])) ||
               ('A' != toupper(str[4])) || ('T' != toupper(str[5])) ||
               ('E' != toupper(str[6]))) {
        return invalid;
    } else if ('\n' == str[7]) {
        return invalid;
    } else if (' ' == str[7]) {
        authenticate.endptr = skip_to_word(str + 7);

        if (eol_found(authenticate.endptr, 0)) {
            return authenticate;
        } else {
            return incomplete;
        }

    } else {
        return invalid;
    }
}

/*----------------------------------------------------------------------------*/

static ParsedMessageType parse_authorize_type(char const *str) {
    ParsedMessageType authorize = {
        .type = W_REQUEST_AUTHORIZE,
    };

    if (0 == str) {
        return invalid;
    } else if ((0 == str[0]) || (0 == str[1]) || (0 == str[2]) ||
               (0 == str[3]) || (0 == str[4])) {
        return incomplete;
    } else if (('R' != toupper(str[0])) || ('I' != toupper(str[1])) ||
               ('Z' != toupper(str[2])) || ('E' != toupper(str[3]))) {
        return invalid;
    } else if ('\n' == str[4]) {
        return invalid;
    } else if (' ' == str[4]) {
        authorize.endptr = skip_to_word(str + 4);

        if (eol_found(authorize.endptr, 0)) {
            return authorize;
        } else {
            return incomplete;
        }

    } else {
        return invalid;
    }
}

/*----------------------------------------------------------------------------*/

static ParsedMessageType parse_request(char const *str) {

    if (0 == str) {

        return invalid;

    } else if ((0 == str[0]) || (0 == str[1]) || (0 == str[2]) ||
               (0 == str[3])) {
        return incomplete;
    } else if (('U' != toupper(str[0])) || ('T' != toupper(str[1])) ||
               ('H' != toupper(str[2]))) {
        return invalid;

    } else if ('E' == toupper(str[3])) {
        return parse_authenticate_type(str + 4);

    } else if ('O' == toupper(str[3])) {
        return parse_authorize_type(str + 4);

    } else {
        return invalid;
    }
}

/*----------------------------------------------------------------------------*/

static ParsedMessageType parse_message_type(char const *str) {
    str = skip_to_word(str);

    if (0 != str) {

        switch (toupper(str[0])) {

            case 0:

                return incomplete;

            case 'R':

                return parse_response(str + 1);

            case 'A':

                return parse_request(str + 1);

            default:

                return invalid;
        }

        // char c = toupper(str[0]);

        // if(0 == c) {

        //    return incomplete;

        //} else if (c == 'R') {
        //    return parse_response(str + 1);

        //} else {
        //    if (c != 'A') {
        //        return invalid;

        //    } else if ((0 == str[1]) || (0 == str[2]) || (0 == str[3]) ||
        //               (0 == str[4])) {
        //        return incomplete;
        //    } else if (('U' != toupper(str[1])) || ('T' != toupper(str[2])) ||
        //               ('H' != toupper(str[3]))) {
        //        return invalid;

        //    } else if ('E' == toupper(str[4])) {
        //        return parse_authenticate_type(str + 5);

        //    } else if ('O' == toupper(str[4])) {
        //        return parse_authorize_type(str + 5);

        //    } else {
        //        return invalid;
        //    }
        //}
    }
}

/*----------------------------------------------------------------------------*/

W_ParsedMessage w_parse_message(char const *msg) {

    msg = skip_to_word(msg);

    if (0 == msg) {
        return W_ParsedInvalid;

    } else if (0 == msg[0]) {

        return W_ParsedIncomplete;

    } else {

        char *endptr = 0;
        long id_long = strtol(msg, &endptr, 10);

        if ((endptr == msg) || (id_long < 0) || (id_long > UINT32_MAX)) {
            return W_ParsedInvalid;

        } else if (*endptr == 0) {
            return W_ParsedIncomplete;

        } else {
            ParsedMessageType type = parse_message_type(endptr);

            W_ParsedMessage result = {
                .type = type.type,
            };

            char const *eol = 0;

            if ((result.type != W_INVALID) && (result.type != W_INCOMPLETE) &&
                eol_found(type.endptr, &eol)) {
                result.id = (uint32_t)id_long;
                result.body = type.endptr;
                result.trailer = eol + 1;
            };
            return result;
        }
    }
}

/*----------------------------------------------------------------------------*/

void w_print_parsed_message(FILE *out, W_ParsedMessage msg) {
    char const *type = "INVALID";

    switch (msg.type) {
        case W_INVALID:
            type = "Invalid";
            break;

        case W_INCOMPLETE:
            type = "Incomplete";
            break;

        case W_REQUEST_AUTHENTICATE:
            type = "Authenticate";
            break;

        case W_REQUEST_AUTHORIZE:
            type = "Authorize";
            break;

        case W_RESPONSE_ERROR:
            type = "Error response";
            break;

        case W_RESPONSE_OK:
            type = "Ok response";
            break;
    };

    fprintf(out,
            "Waechter message %s  id: %" PRIu32
            " -   body: %s    - trailer: %s\n",
            type,
            msg.id,
            strsan(msg.body),
            strsan(msg.trailer));
}

/*----------------------------------------------------------------------------*/
