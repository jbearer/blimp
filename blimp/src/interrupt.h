#ifndef BLIMP_INTERRUPT_H
#define BLIMP_INTERRUPT_H

#include <stdbool.h>

typedef void(*InterruptHandler)(void *);
bool PushInterruptHandler(InterruptHandler handler, void *arg);
void PopInterruptHandler(void);

#endif
