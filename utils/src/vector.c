#include "vector.h"

void Vector_Init(
    Blimp *blimp, Vector *v, size_t elem_size, Destructor destructor)
{
    v->blimp = blimp;
    v->data = NULL;
    v->size = 0;
    v->capacity = 0;
    v->elem_size = elem_size;
    v->destructor = destructor;
}

void Vector_Destroy(Vector *v)
{
    if (v->destructor != NULL) {
        for (size_t i = 0; i < v->size; ++i) {
            v->destructor(Vector_Index(v, i));
        }
    }

    free(v->data);
}

Status Vector_EmplaceBack(Vector *v, void **elem)
{
    TRY(Vector_Reserve(v, ++v->size));
        // Make sure we have space for the new element.
    *elem = Vector_RIndex(v, 0);
        // Return a pointer to the new element.
    return BLIMP_OK;
}

Status Vector_PushBack(Vector *v, const void *elem)
{
    void *space;
    TRY(Vector_EmplaceBack(v, &space));
        // Add the new element.
    memcpy(space, elem, v->elem_size);
        // Initialize it.
    return BLIMP_OK;
}

void Vector_PopBack(Vector *v, void *elem)
{
    assert(v->size > 0);
    memcpy(elem, Vector_RIndex(v, 0), v->elem_size);
        // Copy the last element into `elem`.
    --v->size;
        // Remove it from the vector.
}

Status Vector_Reserve(Vector *v, size_t capacity)
{
    if (capacity <= v->capacity) {
        return BLIMP_OK;
    }

    do {
        v->capacity = 2*v->capacity + 1;
            // +1 so that this is guaranteed to increase the capacity, even if
            // the current capacity is 0.
    } while (v->capacity < capacity);

    return Realloc(v->blimp, v->capacity*v->elem_size, &v->data);
}

Status Vector_Resize(Vector *v, size_t new_size)
{
    if (new_size > v->size) {
        TRY(Vector_Reserve(v, new_size));
    } else {
        if (v->destructor != NULL) {
            for (size_t i = 0; i < v->size - new_size; ++i) {
                v->destructor(Vector_RIndex(v, i));
            }
        }
    }

    v->size = new_size;
    return BLIMP_OK;
}

Status Vector_Contract(Vector *v, size_t delta)
{
    if (delta > v->size) {
        delta = v->size;
    }

    return Vector_Resize(v, v->size - delta);
}
