#---------------------------------------------------------------------
# Stage 1: Build Stage (using a standard Haskell image)
#---------------------------------------------------------------------
# Use an official Haskell image with a specific GHC version for reproducibility
FROM haskell:9.6-slim AS builder
# Or use haskell:latest if you prefer

# Set the working directory inside the container
WORKDIR /app


# Copy stack.yaml and package.yaml or *.cabal and install dependencies
COPY stack.yaml stack.yaml.lock ./
COPY package.yaml ./
RUN stack setup && stack build --only-dependencies

# Copy all source files
COPY . .

# Build the application
RUN stack build --copy-bins


#---------------------------------------------------------------------
# Stage 2: Runtime Stage (using Alpine)
#---------------------------------------------------------------------
# Use a specific Alpine version for stability
FROM alpine:3.21 AS final
# Or use alpine:latest

# --- Install Runtime Dependencies ---
# Haskell programs often dynamically link against GMP, zlib, and libffi on Linux.
# Alpine uses 'musl' libc. 'libc6-compat' might be needed if the binary wasn't
# explicitly built against musl, though often GMP/zlib are the main ones.
RUN apk add --no-cache \
    gmp     \
    libffi  \
    zlib    \
    libc6-compat \
    libstdc++ \
    libgcc 

# --- Application Setup ---
WORKDIR /app

# Copy *only* the compiled executable from the 'builder' stage
# Ensure 'vdb-serv' matches the executable name used in the build stage
COPY --from=builder /app/bin/vdb-serv-exe /app/vdb-serv-exe

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
CMD ["./vdb-serv-exe"]