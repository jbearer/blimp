#ifndef BLIMP_DEBUG_H
#define BLIMP_DEBUG_H

#include "common.h"
#include "hash_map.h"

typedef struct Debugger {
    Blimp *blimp;
    HashMap/*<const Instruction *, Breakpoint>*/ breakpoints;
    const Instruction *ip;
    BlimpStackTrace *trace;
    const BlimpSymbol *command_nt;
    bool stop_at_next_instruction;
    bool resume;
} Debugger;

void Debugger_Init(Debugger *db, const BlimpSymbol *command_nt);
Status Debugger_Attach(Debugger *db, Blimp *blimp);
void Debugger_Detach(Debugger *db);
Blimp *Debugger_GetAttachedBlimp(const Debugger *db);
Status Debugger_Break(
    Debugger *db,
    const Instruction *bp,
    size_t count,
    Expr *condition,
    bool temporary);
Status Debugger_StepI(Debugger *db);
Status Debugger_NextI(Debugger *db);
Status Debugger_Finish(Debugger *db);
Status Debugger_Continue(Debugger *db);
Status Debugger_Bt(Debugger *db);
Status Debugger_List(Debugger *db);
Status Debugger_Repl(Debugger *db);

#endif
