#ifndef BLIMP_INTERRUPT_H
#define BLIMP_INTERRUPT_H

typedef void(*InterruptHandler)(void *);
void OnInterrupt(InterruptHandler handler, void *arg);

#endif
