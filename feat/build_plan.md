# Gitpal Build System: CMake, OpenSSL & Homebrew Integration

This document summarizes our discussion on migrating your gitpal project's build system to CMake, handling its OpenSSL 1.1 dependency, enabling cross-compilation, and packaging it for Homebrew.

## 1. Migrating to CMake for FPC Projects

### Why CMake?

While Free Pascal (FPC) has its own build system (FPMake) and many FPC projects use traditional Makefiles, CMake offers significant advantages for modern cross-platform development, especially when dealing with external C libraries like OpenSSL:

- **Cross-Platform Portability**: CMake generates native build files (Makefiles on Linux/macOS, Visual Studio projects on Windows, Xcode projects on macOS) from a single CMakeLists.txt, ensuring your project can be built consistently across various operating systems.

- **Robust Dependency Finding**: CMake excels at locating external libraries (like OpenSSL) on diverse systems using its `find_package()` modules. This avoids hardcoding paths and makes builds more reliable.

- **Streamlined Build Process**: It abstracts away compiler-specific flags and complex linking commands, making your CMakeLists.txt cleaner and easier to maintain than hand-written Makefiles or complex FPMake configurations for external C dependencies.

- **IDE Integration**: Modern IDEs often have excellent integration with CMake, improving the development experience.

### CMakeLists.txt Breakdown

Here's the structure and key components of your CMakeLists.txt:

# Minimum required CMake version
cmake_minimum_required(VERSION 3.15)

# Define your project.
# We use 'Fortran' as a workaround because CMake historically lacks native 'Pascal' language support.
# We then explicitly tell CMake to use 'fpc' as the Fortran compiler.
project(gitpal VERSION 1.0.0 LANGUAGES Fortran) # *** Manually update this version for each release ***
set(CMAKE_Fortran_COMPILER fpc)

# Define common FPC compiler options (from your original Makefile)
set(FPC_COMMON_COMPILE_OPTIONS
    "-Mdelphi" # Delphi compatibility mode
    "-Sh"      # Short circuits boolean evaluation
    "-O2"      # Optimization level 2
    "-gl"      # Generate debug info for GDB
    "-gw"      # Generate DWARF debug info
    "-Xs"      # Smart link (strip unused code)
    "-fw"      # Handle WideString type
)

# Define paths to your vendored Free Pascal unit directories
set(VENDOR_DIR "${CMAKE_CURRENT_SOURCE_DIR}/vendor") # Assumes 'vendor' directory in project root
set(FPC_UNIT_PATHS
    "-Fu${VENDOR_DIR}/bobaui/v0.1.10" # Path for bobaui units
    "-Fu${VENDOR_DIR}/models"         # Path for models units (assuming 'models' is a directory)
)

# Find OpenSSL 1.1.x specifically
# This tells CMake to look for OpenSSL versions from 1.1.0 up to (but not including) 1.2.0.
# It also ensures both Crypto and SSL components are found.
find_package(OpenSSL 1.1 COMPONENTS Crypto SSL REQUIRED)

# Generate a Pascal unit file with project version for in-app display
# You need a template file: 'src/version.pas.in' (e.g., `const AppVersion = '@PROJECT_VERSION@';`)
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/src/version.pas.in"
    "${CMAKE_CURRENT_BINARY_DIR}/src/version.pas"
    @ONLY
)

# Define all source files, including the dynamically generated version unit
set(GITPAL_SOURCES
    src/gitpal.pas # Main program source
    src/app.pas    # Your unit file
    "${CMAKE_CURRENT_BINARY_DIR}/src/version.pas" # Generated version unit
)

# Create the executable target
add_executable(gitpal ${GITPAL_SOURCES})

# Apply FPC compiler options to the target
target_compile_options(gitpal PRIVATE ${FPC_COMMON_COMPILE_OPTIONS})

# Add FPC unit search paths (-Fu)
target_compile_options(gitpal PRIVATE "-Fu${CMAKE_CURRENT_SOURCE_DIR}/src") # For your 'src' units
foreach(unit_path ${FPC_UNIT_PATHS})
    target_compile_options(gitpal PRIVATE "${unit_path}")
endforeach()
target_compile_options(gitpal PRIVATE "-Fu${CMAKE_CURRENT_BINARY_DIR}/src") # For the generated version unit

# Set FPC unit output path (-FU)
set_property(TARGET gitpal PROPERTY FPC_UNIT_OUTPUT_DIR "${CMAKE_BINARY_DIR}/fpc_units")
target_compile_options(gitpal PRIVATE "-FU${FPC_UNIT_OUTPUT_DIR}")

# Link OpenSSL libraries
# CMake handles converting ${OPENSSL_LIBRARIES} into appropriate -L and -l flags for FPC's linker.
target_link_libraries(gitpal PRIVATE ${OPENSSL_LIBRARIES})

# Tell FPC where to find OpenSSL include files (-Fi)
foreach(inc_dir ${OPENSSL_INCLUDE_DIRS})
    target_compile_options(gitpal PRIVATE "-Fi${inc_dir}")
endforeach()

# Define installation rules (for `cmake --install .`)
install(TARGETS gitpal DESTINATION bin)

# Add a basic test (for `ctest`)
add_test(NAME gitpal_version_test COMMAND gitpal --version)
```

### Cross-Compilation with CMake Toolchain Files

CMake is excellent for cross-compilation. You define target-specific settings (compiler, sysroot, library locations) in a separate toolchain file (.cmake extension), which you pass to CMake at configure time (`-DCMAKE_TOOLCHAIN_FILE=...`). Your main CMakeLists.txt remains largely unchanged.

#### Key aspects for cross-compilation:

- **Toolchain File** (`<target>.cmake`): This file would set `CMAKE_SYSTEM_NAME` (e.g., Linux, Darwin), `CMAKE_SYSTEM_PROCESSOR` (e.g., arm64), and point to your FPC cross-compiler executable.

- **Sysroot** (`CMAKE_SYSROOT`): You'll need a sysroot for your target, containing the target OS's headers and libraries.

- **OpenSSL for Target**: You must have OpenSSL 1.1 cross-compiled for your specific target architecture and operating system. CMake's `find_package(OpenSSL)` will then search within the defined sysroot/toolchain paths.

- **macOS Runtime Linking (rpath)**: When cross-compiling for macOS, you need to embed rpath entries (like `@loader_path`, `@loader_path/../lib`, and common Homebrew paths `/opt/homebrew/opt/openssl@1.1/lib`) into the compiled binary. This tells the macOS dynamic linker (dyld) where to find libraries at runtime. CMake handles this via variables like `CMAKE_INSTALL_RPATH`, `CMAKE_BUILD_WITH_INSTALL_RPATH`, and `CMAKE_MACOSX_RPATH` set in your toolchain file.

#### Crucial Note on OpenSSL and DOS:

While FPC can target DOS, and CMake can be configured for cross-compilation, running OpenSSL 1.1.x (or any modern OpenSSL version) on FreeDOS is not practically possible. OpenSSL's dependencies and modern cryptographic requirements are incompatible with the fundamental architecture and limitations of DOS. You would not find a usable OpenSSL library for DOS that gitpal could link against. Therefore, gitpal's functionality relying on OpenSSL would not be available on a FreeDOS target.

## 2. Building a Homebrew Package

Homebrew is a macOS (and Linux via Linuxbrew) package manager. It uses Ruby-based "formulae" to define how to download, build, and install software.

### Homebrew Formula Structure (gitpal.rb)

Your Homebrew formula (e.g., `gitpal.rb`) would be placed in a Homebrew tap (a Git repository named `homebrew-yourtapname` on GitHub).

```ruby
class Gitpal < Formula
  desc "A command-line tool to simplify Git workflows" # Concise description
  homepage "https://git.sr.ht/~matthewp/gitpal" # Your project's homepage

  # URL to your released source tarball (e.g., from Sourcehut releases)
  # Homebrew expects a versioned tarball URL.
  url "https://git.sr.ht/~matthewp/gitpal/releases/download/v#{version}/gitpal-#{version}.tar.gz"
  version "1.0.0" # *** IMPORTANT: This must match your release version and the URL ***
  sha256 "YOUR_SHA256_CHECKSUM_HERE" # *** IMPORTANT: Get the SHA256 of the exact tarball ***
  license "MIT" # Your software license

  # Build dependencies (tools needed to compile your software)
  depends_on "fpc" => :build # Free Pascal Compiler
  depends_on "cmake" => :build # CMake build system

  # Runtime dependencies (libraries your software links against or needs at runtime)
  depends_on "openssl@1.1" # Specific OpenSSL 1.1 dependency

  def install
    # Set PKG_CONFIG_PATH to help CMake find Homebrew's keg-only openssl@1.1
    ENV["PKG_CONFIG_PATH"] = "#{Formula["openssl@1.1"].opt_lib}/pkgconfig"

    # Standard CMake build steps
    # CMAKE_INSTALL_PREFIX is automatically set by Homebrew to its cellar path.
    system "cmake", "-S", ".", "-B", "build", *std_cmake_args
    system "cmake", "--build", "build"
    system "cmake", "--install", "build"
  end

  test do
    # Simple test to verify installation and basic functionality
    system "#{bin}/gitpal", "--help" # Or "--version"
  end
end
```

### Key Homebrew Formula Concepts:

- **url and version**: For Homebrew, the url must point to a stable, versioned source tarball (e.g., `gitpal-1.0.0.tar.gz`). The version field in the formula should explicitly match this. Homebrew does not dynamically extract versions from Git tags at build time from tarballs.

- **sha256**: A crucial security measure. You must provide the SHA256 checksum of the exact tarball at the url.

- **depends_on**: Declares your project's build and runtime dependencies. Homebrew will install these automatically. `openssl@1.1` specifically requests that version.

- **install block**: Contains the commands Homebrew runs to build and install your software. For CMake, this involves `cmake`, `cmake --build`, and `cmake --install`. Homebrew handles setting the correct installation prefix.

- **test block**: Essential for verifying that your formula successfully built and installed a working executable.

### Homebrew Workflow:

1. **Prepare Release**: Create a versioned release (e.g., v1.0.0) on Sourcehut, and upload your source tarball (e.g., `gitpal-1.0.0.tar.gz`) to this release. Do not rely on Sourcehut's generic `archive/<hash>.tar.gz` links for Homebrew.

2. **Calculate SHA256**: Download your released tarball and get its SHA256 checksum (e.g., `shasum -a 256 gitpal-1.0.0.tar.gz`).

3. **Create/Update Formula**: Place the `gitpal.rb` file in the `Formula/` directory of your Homebrew tap (e.g., `homebrew-gitpal`).

4. **Tap Your Repo**: `brew tap your_github_username/your_tap_repo_name`

5. **Install**: `brew install gitpal`

6. **Audit**: `brew audit --strict --new --online your_github_username/your_tap_repo_name/gitpal` to ensure it follows Homebrew's guidelines.

By following these steps, you will have a robust build system for gitpal that supports cross-platform compilation and can be easily distributed via Homebrew.

---

## Implementation Progress

### ✅ Phase 1: Basic CMake Implementation (COMPLETED)

**Approach Taken**: CMake Custom Commands

After investigating the "Pascal as Fortran" approach suggested in the original plan, we discovered it was problematic due to CMake's language system conflicts. Instead, we successfully implemented a **custom command approach** that provides all the benefits without fighting CMake's internals.

**Implementation Details:**

```cmake
# CMake project with no language specified
project(gitpal VERSION 1.0.0 LANGUAGES NONE)

# Custom command that invokes FPC directly
add_custom_command(
    OUTPUT ${GITPAL_BINARY}
    COMMAND fpc ${FPC_FLAGS} -o${GITPAL_BINARY} src/app.pas
    DEPENDS ${GITPAL_SOURCES}
    COMMENT "Compiling gitpal with Free Pascal Compiler"
    VERBATIM
)

add_custom_target(gitpal ALL DEPENDS ${GITPAL_BINARY})
```

**Key Lessons Learned:**

1. **CMake Language Support Limitations**: The "Fortran workaround" doesn't work reliably because:
   - CMake passes GCC/Clang-specific flags that FPC doesn't understand (`-isysroot`, `-c`, etc.)
   - Pascal source file extensions don't match Fortran expectations
   - Dependency tracking becomes complex

2. **Custom Commands Are Superior**: The custom command approach provides:
   - ✅ Full control over FPC compilation flags
   - ✅ Clean integration with CMake's dependency system
   - ✅ Standard CMake workflow (`cmake -S . -B build && cmake --build build`)
   - ✅ Easy cross-platform configuration

### ✅ Phase 2: OpenSSL Dependency Management (COMPLETED)

**Challenge**: Integrate CMake's `find_package(OpenSSL)` with custom FPC compilation.

**Solution**: Extract discovered paths and manually add to FPC flags:

```cmake
# Find OpenSSL with platform-specific hints
if(APPLE)
    if(EXISTS "/opt/homebrew/opt/openssl@1.1")
        set(OPENSSL_ROOT_DIR /opt/homebrew/opt/openssl@1.1)
    elseif(EXISTS "/usr/local/opt/openssl@1.1")
        set(OPENSSL_ROOT_DIR /usr/local/opt/openssl@1.1)
    endif()
endif()

find_package(OpenSSL 1.1 REQUIRED COMPONENTS Crypto SSL)

# Extract library directory and add to FPC flags
get_filename_component(OPENSSL_LIB_DIR "${OPENSSL_CRYPTO_LIBRARY}" DIRECTORY)
list(APPEND FPC_FLAGS
    "-Fl${OPENSSL_LIB_DIR}"
    "-k-rpath" 
    "-k${OPENSSL_LIB_DIR}"
    "-k-lssl"
    "-k-lcrypto"
)
```

**Results:**
- ✅ **Cross-platform**: Automatically handles Intel vs Apple Silicon Homebrew paths
- ✅ **Version validation**: Ensures OpenSSL 1.1.x requirement
- ✅ **Future-proof**: Easy to extend for Linux/Windows OpenSSL discovery

### ✅ Phase 3: Version Generation from Git Tags (COMPLETED)

Successfully implemented automatic version detection matching the original Makefile behavior:

```cmake
execute_process(
    COMMAND git describe --exact-match --tags HEAD
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_TAG
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

if(GIT_TAG)
    string(REGEX REPLACE "^v" "" GITPAL_VERSION "${GIT_TAG}")
else()
    set(GITPAL_VERSION "development")
endif()

# Generate version file in build directory
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/gitpal-version.inc" 
     "const AppVersion = '${GITPAL_VERSION}';\n")
```

### ✅ Phase 4: Installation Support (COMPLETED)

Added standard CMake installation with proper permissions and uninstall target:

```cmake
install(PROGRAMS ${GITPAL_BINARY} 
        DESTINATION bin
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                   GROUP_READ GROUP_EXECUTE
                   WORLD_READ WORLD_EXECUTE)

# Uninstall target using template
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)

add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
```

### ✅ Phase 5: Full Makefile Migration (COMPLETED)

All functionality from the original Makefile has been successfully migrated:
- ✅ Removed Makefile from repository
- ✅ Moved version generation to build directory
- ✅ Updated README with CMake-only instructions
- ✅ Added .gitignore entry for build/ directory

### 🔄 Phase 6: Cross-Compilation (PLANNED)

#### Requirements for Linux CI → Linux/macOS Binaries

**1. Free Pascal Cross-Compilers**
- **Linux → Linux**: Native FPC compiler (trivial)
- **Linux → macOS**: Need cross-compiler toolchain
  - Intel Mac: `ppcrossx64` compiler
  - Apple Silicon: `ppcrossa64` compiler
  - Built using FPC's cross-compiler build process

**2. Target System Dependencies**

For macOS cross-compilation from Linux:
- **macOS SDK**: Headers and system libraries
  - Typically extracted from Xcode (licensing requires Mac ownership)
  - Can use [osxcross](https://github.com/tpoechtrager/osxcross) project
- **OpenSSL 1.1 for macOS**: Pre-built libraries for target architecture
- **System libraries**: Core `.dylib` files from macOS

**3. CMake Configuration Changes**

Would need toolchain files for each target:

```cmake
# darwin-x86_64.cmake
set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_PROCESSOR x86_64)
set(CMAKE_Pascal_COMPILER /path/to/ppcrossx64)
set(CMAKE_SYSROOT /path/to/macos-sdk)
set(CMAKE_FIND_ROOT_PATH /path/to/macos-libs)
```

**4. Key Challenges**

1. **Legal/Licensing**: Apple SDK EULA restricts usage to Apple hardware
2. **Complexity**: Managing SDKs, cross-compilers, and libraries
3. **Code Signing**: macOS may require signed binaries for distribution
4. **Testing**: Cannot test macOS binaries on Linux CI directly

**5. Alternative Approaches**

More practical solutions:
- **GitHub Actions Matrix Builds**: Use native runners for each OS
  ```yaml
  strategy:
    matrix:
      os: [ubuntu-latest, macos-latest]
  ```
- **Separate CI Jobs**: Build on platform-specific runners
- **Docker + QEMU**: For Linux targets only (macOS not viable)

**Recommendation**: Use platform-specific CI runners instead of cross-compilation for macOS targets. This avoids legal complexities and ensures proper testing.

**Updated Assessment:**

The custom command approach has **exceeded expectations** for achieving our goals:

- **✅ Cross-compilation Ready**: Platform-specific logic can be easily added
- **✅ Homebrew Compatible**: Standard CMake workflow that Homebrew expects
- **✅ Maintainable**: Cleaner than the "Fortran hack" and easier than hybrid approaches
- **✅ Extensible**: Easy to add more dependencies, platforms, and build configurations

## Comparison: CMake Custom Commands vs Original Plan

| Aspect | Original Plan (Fortran Hack) | Implemented (Custom Commands) |
|--------|------------------------------|--------------------------------|
| **Complexity** | High (fighting language system) | Low (clean separation) |
| **Maintainability** | Poor (fragile workarounds) | Excellent (explicit control) |
| **Cross-platform** | Difficult (compiler flag conflicts) | Easy (conditional logic) |
| **Dependency Finding** | Good (standard CMake) | Excellent (standard + manual control) |
| **Homebrew Integration** | Good | Excellent |
| **Build Performance** | Same | Same |
| **Learning Curve** | High (CMake language internals) | Low (standard CMake + FPC) |

**Recommendation**: Continue with the custom command approach for all remaining implementation phases.
