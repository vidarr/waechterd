/**

        @author         Michael J. Beer

*/

/*----------------------------------------------------------------------------*/

#include "waechter.h"
#include <assert.h>

#include "test_helpers.c"

/*----------------------------------------------------------------------------*/

static bool next_message_is(W_MessageBuffer *buffer,
                            uint32_t id,
                            W_MessageType type,
                            char const *body) {

    W_Message msg = w_message_buffer_next(buffer);

    bool ok = message_is(msg, id, type, body);

    w_message_clear(&msg);

    return ok;
}

/*----------------------------------------------------------------------------*/

static void test_w_message_buffer() {

    W_MessageBuffer *buffer = w_message_buffer();

    assert(0 != buffer);

    buffer = w_message_buffer_free(buffer);
}

/*----------------------------------------------------------------------------*/

static void test_w_message_buffer_free() {

    assert(0 == w_message_buffer_free(0));

    W_MessageBuffer *buffer = w_message_buffer();

    assert(0 != buffer);

    buffer = w_message_buffer_free(buffer);
    assert(0 == buffer);
}

/*----------------------------------------------------------------------------*/

static void test_w_message_buffer_add() {

    assert(!w_message_buffer_add(0, 0));

    W_MessageBuffer *buffer = w_message_buffer();

    assert(0 != buffer);

    assert(!w_message_buffer_add(buffer, 0));
    assert(!w_message_buffer_add(0, "1021 abra cadabra"));
    assert(w_message_buffer_add(buffer, "1021 abra cadabra"));

    assert(w_message_buffer_add(buffer, "simsalabim"));
    assert(w_message_buffer_add(buffer, "dreimal schwarzer kater"));

    buffer = w_message_buffer_free(buffer);
}

/*----------------------------------------------------------------------------*/

static void test_w_message_buffer_next() {

    assert(message_is(w_message_buffer_next(0), W_INVALID, 0, 0));

    W_MessageBuffer *buffer = w_message_buffer();

    assert(0 != buffer);

    assert(w_message_buffer_add(buffer, "1021 "));
    assert(next_message_is(buffer, W_INCOMPLETE, 0, 0));

    assert(w_message_buffer_add(buffer, "tomentella\n"));
    assert(next_message_is(buffer, W_INVALID, 0, 0));

    // Garbage line should have been removed

    assert(next_message_is(buffer, W_INCOMPLETE, 0, 0));

    assert(w_message_buffer_add(buffer, "1021 "));
    assert(next_message_is(buffer, W_INCOMPLETE, 0, 0));

    assert(w_message_buffer_add(buffer, "authorize a1732 1.2"));
    assert(next_message_is(buffer, W_INCOMPLETE, 0, 0));

    assert(w_message_buffer_add(buffer, "3.2\n"));
    assert(
        next_message_is(buffer, W_REQUEST_AUTHORIZE, 1021, "a1732 1.23.2\n"));

    assert(w_message_buffer_add(buffer, "20111 authen"));
    assert(w_message_buffer_add(buffer, "ticate plain anto"));
    assert(w_message_buffer_add(buffer, "n Wolke17\n    38"));
    assert(w_message_buffer_add(
        buffer, "   r:ok   \n13211 authorize 182cbA 3.2.1.14"));

    assert(next_message_is(
        buffer, W_REQUEST_AUTHENTICATE, 20111, "plain anton Wolke17\n"));

    assert(next_message_is(buffer, W_RESPONSE_OK, 38, "\n"));

    assert(next_message_is(buffer, W_INCOMPLETE, 0, 0));

    assert(w_message_buffer_add(buffer, ".17  \n nabcABC "));

    assert(next_message_is(
        buffer, W_REQUEST_AUTHORIZE, 13211, "182cbA 3.2.1.14.17  \n"));

    buffer = w_message_buffer_free(buffer);
    assert(0 == buffer);
}

/*----------------------------------------------------------------------------*/

int main() {
    test_w_message_buffer();
    test_w_message_buffer_free();
    test_w_message_buffer_add();
    test_w_message_buffer_next();
}

/*----------------------------------------------------------------------------*/
