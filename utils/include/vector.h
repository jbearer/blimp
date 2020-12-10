#ifndef BLIMP_VECTOR_H
#define BLIMP_VECTOR_H

#include "blimp.h"
#include "common.h"

typedef void(*Destructor)(void *);

typedef struct {
    Blimp *blimp;
    void *data;
    size_t size;
    size_t capacity;
    size_t elem_size;
    Destructor destructor;
} Vector;

static inline Blimp *Vector_GetBlimp(const Vector *v)
{
    return v->blimp;
}

/**
 * \brief Create an empty vector.
 *
 * \param elem_size
 *      The size in bytes of elements which will be added to this vector.
 * \param destructor
 *      An optional destructor which will be called to clean up individual
 *      elements when the Vector is destroyed, or when elements are removed
 *      without being returned, such as in Vector_Contract(). If `NULL`,
 *      elements are not destroyed by the Vector.
 */
PRIVATE void Vector_Init(
    Blimp *blimp, Vector *v, size_t elem_size, Destructor destructor);

/**
 * \brief Destroy a vector.
 *
 * Cleans up resources allocated by the vector, and calls the destructor (if
 * non-NULL) on each element remaining in the vector.
 */
PRIVATE void Vector_Destroy(Vector *v);

/**
 * \brief Append a new, uninitialized element and return a pointer to it.
 */
PRIVATE Status Vector_EmplaceBack(Vector *v, void **elem);

/**
 * \brief Append a new element and initialize it by copying from `elem`.
 */
PRIVATE Status Vector_PushBack(Vector *v, const void *elem);

/**
 * \brief Remove the last element and copy its contents into `elem`.
 *
 * \pre `v` is not empty.
 */
PRIVATE void Vector_PopBack(Vector *v, void *elem);

/**
 * \brief Reserve enough space for `capacity` elements.
 *
 * This function only allocates space for elements to be added later. It does
 * not change the size of the vector.
 */
PRIVATE Status Vector_Reserve(Vector *v, size_t capacity);

/**
 * \brief Change the size of the vector.
 *
 * If `new_size` is greater than the current size of `v`, then `new_size - size`
 * new, uninitialized elements are appended, as if by Vector_EmplaceBack().
 *
 * If `new_size` is less than the current size of `v`, then `size - new_size`
 * elements are removed from the end of the vector. If provided, the destructor
 * is called on each of these elements.
 */
PRIVATE Status Vector_Resize(Vector *v, size_t new_size);

/**
 * \brief Shrink the vector by removing `delta` elements from the end.
 *
 * The destrucvtor (if provided) is called on each removed element.
 *
 * If `delta` is greater than the current size of `v`, all elements are removed.
 */
PRIVATE Status Vector_Contract(Vector *v, size_t delta);

static inline size_t Vector_Length(const Vector *v)
{
    return v->size;
}

static inline bool Vector_Empty(const Vector *v)
{
    return v->size == 0;
}

static inline void *Vector_Index(const Vector *v, size_t i)
{
    assert(i < v->size);
    return (char *)v->data + i*v->elem_size;
}

static inline void *Vector_RIndex(const Vector *v, size_t i)
{
    return Vector_Index(v, Vector_Length(v) - 1 - i);
}

static inline void *Vector_Begin(const Vector *v)
{
    return v->data;
}

static inline void *Vector_End(const Vector *v)
{
    return (char *)v->data + v->size*v->elem_size;
}

static inline void *Vector_Next(const Vector *v, void *p)
{
    assert(Vector_Begin(v) <= p && p < Vector_End(v));
    return (char *)p + v->elem_size;
}

////////////////////////////////////////////////////////////////////////////////
// Destructors for common data types
//

static inline void FreeDestructor(void *pp)
{
    free(*(void **)pp);
}

static inline void ExprDestructor(void *p)
{
    Blimp_FreeExpr(*(Expr **)p);
}


#endif
