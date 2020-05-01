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

int main(int argc, char *const *argv)
{
    struct option options[] = {
        {"help",    no_argument, NULL, 'h'},
        {"version", no_argument, NULL, 'v'},
        {0, 0, 0, 0},
    };

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "hv", options, &i)) != -1) {
        switch (option) {
            case 'h':
                PrintUsage(stdout, argc, argv);
                return EXIT_SUCCESS;
            case 'v':
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

    Blimp *blimp = Blimp_New();
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
