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

Status Vector_Copy(const Vector *from, Vector *to, CopyFunc copy)
{
    Vector_Init(from->blimp, to, from->elem_size, from->destructor);
    TRY(Vector_Reserve(to, from->size));

    if (copy == NULL) {
        memcpy(to->data, from->data, from->size*from->elem_size);
        to->size = from->size;
    } else {
        for (size_t i = 0; i < from->size; ++i) {
            if (copy(
                    (char *)from->data + i*from->elem_size,
                    (char *)to->data   + i*from->elem_size
                ) != BLIMP_OK)
            {
                Vector_Destroy(to);
            }
            ++to->size;
        }
    }

    return BLIMP_OK;
}

Status Vector_Split(Vector *from, size_t i, Vector *to)
{
    assert(i < from->size);

    Vector_Init(from->blimp, to, from->elem_size, from->destructor);
    TRY(Vector_Reserve(to, from->size - i));
    memcpy(
        to->data,
        (char *)from->data + i*from->elem_size,
        (from->size - i)*from->elem_size
    );
    to->size = from->size - i;
    from->size = i;
    return BLIMP_OK;
}

void Vector_Clear(Vector *v)
{
    CHECK(Vector_Resize(v, 0));
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

void Vector_Shift(Vector *v, size_t n)
{
    if (n > v->size) {
        n = v->size;
    }

    if (v->destructor != NULL) {
        for (size_t i = 0; i < n; ++i) {
            v->destructor(Vector_Index(v, i));
        }
    }

    memmove(
        v->data,
        (char *)v->data + v->elem_size*n,
        v->elem_size*(v->size - n)
    );

    v->size -= n;
}

Status Vector_Contract(Vector *v, size_t delta)
{
    if (delta > v->size) {
        delta = v->size;
    }

    return Vector_Resize(v, v->size - delta);
}
