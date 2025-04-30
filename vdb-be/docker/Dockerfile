#---------------------------------------------------------------------
# Stage 1: Build Stage (using a standard Haskell image)
#---------------------------------------------------------------------
# Use an official Haskell image with a specific GHC version for reproducibility
FROM haskell:9.6 AS builder
# Or use haskell:latest if you prefer

# Set the working directory inside the container
WORKDIR /app

# --- Dependency Caching ---
# Copy the Cabal configuration files first
COPY *.cabal ./
# Copy cabal.project or cabal.project.freeze if they exist
COPY cabal.project* ./

# Install dependencies only. This layer is cached if config files don't change.
# Update the package database
RUN cabal update
# Build only the dependencies
RUN cabal build --only-dependencies

# --- Build Application ---
# Copy the rest of the application source code
COPY . .

# Build the executable.
# Using 'install' places the binary in a predictable location.
# Adjust optimization flags (-O2) as needed.
RUN cabal install exe:rr-server --install-method=copy --installdir=/app/bin --overwrite-policy=always -O2
# Note: For truly minimal Alpine images, you might investigate static linking
# by adding ghc-options like '-static -optl-static -optl-pthread' in your .cabal file,
# but this can be complex and platform-dependent. Dynamic linking (default)
# requires installing runtime dependencies in the final stage.

#---------------------------------------------------------------------
# Stage 2: Runtime Stage (using Alpine)
#---------------------------------------------------------------------
# Use a specific Alpine version for stability
FROM alpine:3.20 AS final
# Or use alpine:latest

# --- Install Runtime Dependencies ---
# Haskell programs often dynamically link against GMP, zlib, and libffi on Linux.
# Alpine uses 'musl' libc. 'libc6-compat' might be needed if the binary wasn't
# explicitly built against musl, though often GMP/zlib are the main ones.
RUN apk add --no-cache \
    gmp     \
    musl    \
    gcompat \
    libffi  \
    zlib    \
    libc6-compat \
    libstdc++ \
    libgcc 

# --- Application Setup ---
WORKDIR /app

# Copy *only* the compiled executable from the 'builder' stage
# Ensure 'rr-server' matches the executable name used in the build stage
COPY --from=builder /app/bin/rr-server /app/rr-server

# --- Security Best Practice: Run as non-root user ---
# Create a group and user
RUN addgroup -S appgroup && adduser -S appuser -G appgroup
# Set the user for subsequent commands
USER appuser

# --- Runtime Configuration ---
# Expose the port the server listens on
EXPOSE 3000

# Command to run the executable when the container starts
# Ensure the executable path is correct within the WORKDIR
CMD ["./rr-server"]