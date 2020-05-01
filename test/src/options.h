#ifndef TEST_OPTIONS
#define TEST_OPTIONS

typedef enum {
    VERB_NONE,
    VERB_SUITE,
    VERB_GROUP,
    VERB_TEST,
    VERB_DEBUG,
    MAX_VERBOSITY,
} Verbosity;

typedef struct {
    Verbosity verbosity;
    const char *filter;
    bool use_racket;
    size_t racket_timeout; // ms
} Options;

#endif
