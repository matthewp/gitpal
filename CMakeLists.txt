# Minimum required CMake version
cmake_minimum_required(VERSION 3.15)

# Define project - no language needed for custom commands
project(gitpal VERSION 1.0.0 LANGUAGES NONE)

# Define common FPC compiler options (matching current Makefile)
set(FPC_FLAGS
    "-O2"      # Optimization level 2 (matches Makefile)
)

# Define paths to vendored Free Pascal unit directories
set(VENDOR_DIR "${CMAKE_CURRENT_SOURCE_DIR}/vendor")
list(APPEND FPC_FLAGS
    "-Fu${VENDOR_DIR}/bobaui/v0.1.15"  # Path for bobaui units
    "-Fu${VENDOR_DIR}/models.pas/v0.0.3"  # Path for models units  
    "-Fi${VENDOR_DIR}/models.pas/v0.0.3"  # Include path for models units
    "-Fu${CMAKE_CURRENT_SOURCE_DIR}/src"  # Source directory
)

# Find OpenSSL 1.1.x specifically
# On macOS with Homebrew, OpenSSL 1.1 is keg-only, so we need to help CMake find it
if(APPLE)
    # Try both Intel and Apple Silicon Homebrew paths
    if(EXISTS "/opt/homebrew/opt/openssl@1.1")
        set(OPENSSL_ROOT_DIR /opt/homebrew/opt/openssl@1.1)
    elseif(EXISTS "/usr/local/opt/openssl@1.1")
        set(OPENSSL_ROOT_DIR /usr/local/opt/openssl@1.1)
    endif()
endif()

find_package(OpenSSL 1.1 REQUIRED COMPONENTS Crypto SSL)

# Debug: Show what OpenSSL CMake found (can be removed later)
message(STATUS "Found OpenSSL ${OPENSSL_VERSION} at ${OPENSSL_LIB_DIR}")

# Add OpenSSL library paths and linking flags for FPC
get_filename_component(OPENSSL_LIB_DIR "${OPENSSL_CRYPTO_LIBRARY}" DIRECTORY)
list(APPEND FPC_FLAGS
    "-Fl${OPENSSL_LIB_DIR}"
    "-k-rpath" 
    "-k${OPENSSL_LIB_DIR}"
    "-k-lssl"
    "-k-lcrypto"
)



# Define source files that the build depends on
set(GITPAL_SOURCES
    src/gitpal.pas
    src/command_commit.pas
    src/command_changelog.pas
    src/command_setup.pas
    src/git.pas
    src/gitpal_auth.pas
    src/oauth_client.pas
    src/oauth_server.pas
    src/token_storage.pas
    src/browser_launch.pas
    src/config_manager.pas
    src/provider_config.pas
    src/logging.pas
    # Add other source dependencies here as needed
)

# Ensure bin directory exists
file(MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")

# Define the output binary path
set(GITPAL_BINARY "${CMAKE_CURRENT_SOURCE_DIR}/bin/gitpal")

# Custom command to compile with FPC
add_custom_command(
    OUTPUT ${GITPAL_BINARY}
    COMMAND fpc ${FPC_FLAGS} -o${GITPAL_BINARY} ${CMAKE_CURRENT_SOURCE_DIR}/src/gitpal.pas
    DEPENDS ${GITPAL_SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Compiling gitpal with Free Pascal Compiler"
    VERBATIM
)

# Create a target that depends on the custom command
add_custom_target(gitpal ALL DEPENDS ${GITPAL_BINARY})

# Installation configuration
install(PROGRAMS ${GITPAL_BINARY} 
        DESTINATION bin
        COMPONENT Runtime
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                   GROUP_READ GROUP_EXECUTE
                   WORLD_READ WORLD_EXECUTE)

# Set default installation prefix if not specified
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "Default install prefix" FORCE)
endif()

# Show installation configuration
message(STATUS "Installation prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "Binary will be installed to: ${CMAKE_INSTALL_PREFIX}/bin/gitpal")

# Add uninstall target
if(NOT TARGET uninstall)
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        IMMEDIATE @ONLY)

    add_custom_target(uninstall
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()
