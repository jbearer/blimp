# Search for the path containing library's headers
find_path(Readline_ROOT_DIR
    NAMES include/readline/readline.h
)

# Search for include directory
find_path(Readline_INCLUDE_DIR
    NAMES readline/readline.h
    HINTS ${Readline_ROOT_DIR}/include
)

# Search for library
find_library(Readline_LIBRARY
    NAMES readline
    HINTS ${Readline_ROOT_DIR}/lib
)

# Conditionally set READLINE_FOUND value
if(Readline_INCLUDE_DIR AND Readline_LIBRARY)
  set(Readline_FOUND TRUE)
endif()