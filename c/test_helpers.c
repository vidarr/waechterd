/**

        @author         Michael J. Beer

*/

#include <stdbool.h>
#include <string.h>
#include "waechter_message.h"

/*----------------------------------------------------------------------------*/

static bool strequal(char const *s1, char const *s2) {
    return (s1 == s2) || ((0 != s1) && (0 != s2) && (0 == strcmp(s1, s2)));
}

/*----------------------------------------------------------------------------*/

static bool message_is(W_Message msg, W_MessageType type, uint32_t id,
                       char const *body) {

    return (msg.type == type) && (msg.id == id) &&
           strequal(body, msg.body);
}

/*----------------------------------------------------------------------------*/
