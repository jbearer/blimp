#ifndef BLIMP_READLINE_H
#define BLIMP_READLINE_H

#include "common.h"
#include "options.h"

void Readline_Init(const Options *options);
char *Readline(const char *prompt);
Expr *Readline_ReadExpr(Blimp *blimp, const char *prompt, bool blank_line_repeats);
void Readline_SaveHistory(const Options *options);

#endif
