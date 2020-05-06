#include <getopt.h>
#include <unistd.h>

#include "blimp.h"

#define VERSION_MAJOR 0
#define VERSION_MINOR 1
#define VERSION_PATCH 0

static void PrintVersion(FILE *f)
{
    fprintf(f, "The glorious bl:mp execution environment, version %d.%d.%d\n",
        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
}

static void PrintUsage(FILE *f, int argc, char *const *argv)
{
    (void)argc;

    PrintVersion(f);
    fprintf(f, "\n");
    fprintf(f, "Usage: %s [options] <file>\n", argv[0]);
    fprintf(f, "Options:\n");
    fprintf(f, "    -h, --help\n");
    fprintf(f, "        Show this help and exit\n");
    fprintf(f, "    -v, --version\n");
    fprintf(f, "        Print version information\n");
}

typedef enum {
    FLAG_HELP               = 'h',
    FLAG_VERSION            = 'v',

    FLAG_NO_SHORT_OPTION    = 'z'+1,
        // Dummy option which should be greater than all the short options
        // specified above. Any flag declared after this sentinel with no
        // explicit short option value will be automatically assigned a value
        // greater than the value of the sentinel, which should guarantee that
        // all the flags have unique values.

    FLAG_OBJECT_POOL_BATCH_SIZE,
} Flag;

int main(int argc, char *const *argv)
{
    struct option options[] = {
        {"object-pool-batch-size",  required_argument,  NULL, FLAG_OBJECT_POOL_BATCH_SIZE},
        {"help",                    no_argument,        NULL, FLAG_HELP},
        {"version",                 no_argument,        NULL, FLAG_VERSION},
        {0, 0, 0, 0},
    };

    BlimpOptions blimp_options = DEFAULT_BLIMP_OPTIONS;

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "hv", options, &i)) != -1) {
        switch (option) {
            case FLAG_OBJECT_POOL_BATCH_SIZE: {
                char *invalid;
                blimp_options.object_pool_batch_size = strtol(
                    optarg, &invalid, 0);
                if (!*optarg || *invalid) {
                    fprintf(stderr,
                        "object-pool-batch-size: argument must be an integer\n");
                    PrintUsage(stderr, argc, argv);
                    return EXIT_FAILURE;
                }
                break;
            }

            case FLAG_HELP:
                PrintUsage(stdout, argc, argv);
                return EXIT_SUCCESS;

            case FLAG_VERSION:
                PrintVersion(stdout);
                return EXIT_SUCCESS;

            default:
                PrintUsage(stderr, argc, argv);
                return EXIT_FAILURE;
        }
    }

    if (i >= argc) {
        PrintUsage(stderr, argc, argv);
        return 1;
    }
    const char *file = argv[i];

    Blimp *blimp = Blimp_New(&blimp_options);
    if (blimp == NULL) {
        fprintf(stderr, "bl:mp: unable to initialize interpreter\n");
        return EXIT_FAILURE;
    }

    BlimpExpr *expr;
    Blimp_Check(Blimp_ParseFile(blimp, file, &expr));
    Blimp_DumpExpr(stdout, expr);
    fprintf(stdout, "\n");
    return EXIT_SUCCESS;
}
