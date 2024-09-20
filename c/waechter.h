/**

        @author Michael J. Beer

*/
#ifndef WAECHTER_H
#define WAECHTER_H


#include <stdbool.h>

/*----------------------------------------------------------------------------*/

#include "waechter_message.h"

struct W_MessageBufferStruct;

typedef struct W_MessageBufferStruct W_MessageBuffer;

/*----------------------------------------------------------------------------*/

W_MessageBuffer *w_message_buffer();

W_MessageBuffer *w_message_buffer_free(W_MessageBuffer *self);

bool w_message_buffer_add(W_MessageBuffer *self, char const *segment);

W_Message w_message_buffer_next(W_MessageBuffer *self);

/*----------------------------------------------------------------------------*/



/*----------------------------------------------------------------------------*/
#endif

