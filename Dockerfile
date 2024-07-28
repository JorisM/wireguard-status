# Stage 1: Build the Haskell application
FROM haskell:9.6.5-slim AS haskell-build

# Install system dependencies
RUN apt-get update && apt-get install -y \
    docker.io \
    npm \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory in the container
WORKDIR /app

# First Build the deps for better caching
COPY backend/stack.yaml .
COPY backend/stack.yaml.lock .
COPY backend/wireguard-status.cabal .
RUN stack install --only-dependencies

# Then build the app itself
COPY backend/app app
COPY backend/src src
COPY backend/test test
COPY backend/Setup.hs .
COPY backend/package.yaml .
COPY README.md .
COPY CHANGELOG.md .
COPY LICENSE .
COPY static static
RUN stack build --copy-bins

# Stage 2: Build the npm application
FROM node:22-slim AS npm-build

# Install system dependencies
RUN apt-get update && apt-get install -y \
    npm \
    python3 python3-pip \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g spago@next esbuild purescript

# Set the working directory in the container
WORKDIR /app/frontend

# Copy the npm source files
COPY frontend .

RUN mkdir -p /app/static

# Build the npm application
RUN spago bundle --platform browser --bundle-type app --outfile index.js

# Stage 3: Final stage
FROM haskell:9.6.5-slim

# Install system dependencies
RUN apt-get update && apt-get install -y \
    docker.io \
    npm \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory in the container
WORKDIR /app

# Copy the Haskell build output from the haskell-build stage
COPY --from=haskell-build /root/.local/bin/wireguard-status-exe /usr/local/bin/
COPY --from=haskell-build /app/static/index.html /usr/local/bin/static/index.html
# Copy the npm build output from the npm-build stage
COPY --from=npm-build /app/frontend/index.js  /usr/local/bin/static/index.js

# Expose the necessary port
EXPOSE 8888

# Run the Haskell application
CMD ["wireguard-status-exe"]
