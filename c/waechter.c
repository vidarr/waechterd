/**

        @author         Michael J. Beer

*/

#include "waechter.h"
#include "waechter_message_parser.h"
#include <stdlib.h>
#include <string.h>

/*----------------------------------------------------------------------------*/

struct W_MessageBufferStruct {

    char *buffer;
    size_t capacity;

    char *write_ptr;
    size_t remaining_size;

    bool drop_current_line;
};

/*----------------------------------------------------------------------------*/

W_MessageBuffer *w_message_buffer() {

    W_MessageBuffer *self = calloc(1, sizeof(W_MessageBuffer));

    self->capacity = 500;
    self->buffer = calloc(1, self->capacity);
    self->write_ptr = self->buffer;
    self->remaining_size = self->capacity;

    return self;
}

/*----------------------------------------------------------------------------*/

W_MessageBuffer *w_message_buffer_free(W_MessageBuffer *self) {

    if (0 != self) {

        free(self->buffer);
        free(self);
        self = 0;
    }

    return self;
}

/*----------------------------------------------------------------------------*/

static size_t w_strlen(char const *str) {

    if (0 == str) {
        return 0;
    } else {
        return strlen(str);
    }
}

/*----------------------------------------------------------------------------*/

static char *copy_line(char const *str) {

    char *copy = 0;

    if (0 != str) {

        size_t i = 0;

        for (; (0 != str[i]) && ('\n' != str[i]); ++i)
            ;

        if (0 != str[i]) {
            char *copy = calloc(1, i + 1 + 1);
            memcpy(copy, str, i + 1);
            return copy;
        }
    }

    return copy;
}

/*----------------------------------------------------------------------------*/

static bool enhance_if_required(W_MessageBuffer *self, size_t len) {

    if (0 == self) {

        return false;

    } else {

        if (self->remaining_size < len) {

            size_t extend_len = len - self->remaining_size;
            self->capacity += extend_len;
            self->remaining_size += extend_len;
            self->buffer = realloc(self->buffer, self->capacity);
        }

        return true;
    }
}

/*----------------------------------------------------------------------------*/

bool w_message_buffer_add(W_MessageBuffer *self, char const *segment) {

    size_t segment_len = w_strlen(segment);

    if (0 == segment_len) {

        return false;

    } else if (enhance_if_required(self, segment_len)) {

        strcpy(self->write_ptr, segment);
        self->write_ptr += segment_len;
        self->remaining_size -= segment_len;

        return true;

    } else {

        return false;
    }
}

/*----------------------------------------------------------------------------*/

static bool drop_line(W_MessageBuffer *self) {

    if ((0 == self) || (0 == self->buffer)) {

        return false;

    } else {

        size_t i = 0;
        for (; (0 != self->buffer[i]) && ('\n' != self->buffer[i]); ++i)
            ;

        bool ok = '\n' == self->buffer[i];

        char *new_buffer = calloc(1, self->capacity);
        size_t len = self->capacity - self->remaining_size;
        len -= i + 1;
        memcpy(new_buffer, self->buffer + i + 1, len);
        free(self->buffer);
        self->buffer = new_buffer;
        self->write_ptr = self->buffer + len;
        self->remaining_size += i;

        return ok;
    }
}

/*----------------------------------------------------------------------------*/

static bool clean_up_buffer(W_MessageBuffer *self, W_ParsedMessage msg) {

    if ((0 == self) || (0 == self->buffer) || (0 == msg.trailer) ||
        (msg.trailer > self->buffer + self->capacity)) {

        return false;

    } else {

        char *new_buffer = calloc(1, self->capacity);

        size_t len_to_drop = msg.trailer - self->buffer;
        len_to_drop += (msg.trailer[0] == 0);

        size_t len_to_keep =
            self->capacity - self->remaining_size - len_to_drop;

        memcpy(new_buffer, msg.trailer, len_to_keep);
        free(self->buffer);
        self->buffer = new_buffer;
        self->write_ptr = self->buffer + len_to_keep;
        self->remaining_size = self->capacity - len_to_keep;

        return true;
    }
}

/*----------------------------------------------------------------------------*/

W_Message w_message_buffer_next(W_MessageBuffer *self) {

    W_Message result = {
        .type = W_INVALID,
    };

    if ((0 == self) || (0 == self->buffer)) {
        return W_Invalid;

    } else {

        if (self->drop_current_line) {
            self->drop_current_line = !drop_line(self);
        }

        if (self->drop_current_line) {

            return W_Incomplete;

        } else {

            W_ParsedMessage msg = w_parse_message(self->buffer);

            switch (msg.type) {

                case W_INCOMPLETE:
                    result.type = W_INCOMPLETE;
                    break;
                case W_INVALID:
                    self->drop_current_line = !drop_line(self);
                    result.type = W_INVALID;
                    break;

                case W_REQUEST_AUTHORIZE:
                case W_REQUEST_AUTHENTICATE:
                case W_RESPONSE_OK:
                case W_RESPONSE_ERROR:

                    result.type = msg.type;
                    result.id = msg.id;
                    result.body = copy_line(msg.body);
                    clean_up_buffer(self, msg);
                    break;
            };

            return result;
        }
    }
}

/*----------------------------------------------------------------------------*/
